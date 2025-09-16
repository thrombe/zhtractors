#version 460

#include <common.glsl>
#include <uniforms.glsl>

struct GpuState {
    int particle_count;
    int seed_id;

    int bad_flag;

    vec4 _pad_aligned;
};

vec3 quad_verts[6] = vec3[6](
    vec3(1.0, 1.0, 0.0),
    vec3(-1.0, 1.0, 0.0),
    vec3(1.0, -1.0, 0.0),
    vec3(1.0, -1.0, 0.0),
    vec3(-1.0, 1.0, 0.0),
    vec3(-1.0, -1.0, 0.0)
);
vec2 quad_uvs[6] = vec2[6](
    vec2(1.0, 1.0),
    vec2(0.0, 1.0),
    vec2(1.0, 0.0),
    vec2(1.0, 0.0),
    vec2(0.0, 1.0),
    vec2(0.0, 0.0)
);

#ifdef COMPUTE_PASS
    #define bufffer buffer
#else
    #define bufffer readonly buffer
#endif

layout(set = 0, binding = _bind_camera) uniform Ubo {
    Uniforms ubo;
};
layout(set = 0, binding = _bind_scratch) bufffer ScratchBuffer {
    GpuState state;
};
layout(set = 0, binding = _bind_particle_types) bufffer ParticleTypeBuffer {
    ParticleType particle_types[];
};
layout(set = 0, binding = _bind_particles) bufffer ParticleBuffer {
    Particle particles[];
};
layout(set = 0, binding = _bind_particles_draw_call) bufffer ParticlesDrawCallBuffer {
    DrawCall draw_call;
};
layout(push_constant) uniform PushConstantsUniform {
    PushConstants push;
};

void set_seed(int id) {
    seed = int(ubo.frame.frame) ^ id ^ floatBitsToInt(ubo.frame.time) ^ push.seed;
}

// TODO: try applying multiple attractors at the same time
vec3 attractor(vec3 pos) {
    float a, b, c, d, e, f;
    float x = pos.x, y = pos.y, z = pos.z;
    float dt = ubo.params.delta;
 
    float dx, dy, dz;

    // a = 0.00 + ubo.frame.time * 0.0009;
    a = 0.16;
    dx = (-a*x + sin(y)) * dt;
    dy = (-a*y + sin(z)) * dt;
    dz = (-a*z + sin(x)) * dt;

//     a = 0.80, b = 0.7, c = 0.6;
// d = 3.5, e = 0.1;
// dx = ((z-b) * x - d*y) * dt;
// dy = (d * x + (z-b) * y) * dt;
// dz = (c + a*z - ((z*z*z) / 3.0) - (x*x) + e * z * (x*x*x)) * dt;

// a = 40.0, b = 1.833, c = 0.16;
// d = 0.65, e = 55.0, f = 20.0;
// dx = (a*(y-x) + c*x*z) * dt;
// dy = (e*x + f*y - x*z) * dt;
// dz = (b*z + x*y - d*x*x) * dt;

// a = 3.0, b = 2.7, c = 1.7;
// d = 2.0, e = 9.0;
// dx = (y- a*x +b*y*z) * dt;
// dy = (c*y -x*z +z) * dt;
// dz = (d*x*y - e*z) * dt;

// a = -5.5, b = 3.5, d = -1.0;
// dx = y * dt;
// dy = z * dt;
// dz = (-a*x -b*y -z + (d * (pow(x, 3.0)))) * dt;

    return vec3(dx, dy, dz);
}

// vec3 attractor(vec3 p) {
//     float dt    = 0.0001;    // Time step
//     float sigma = 10.0;
//     float rho   = 28.0;
//     float beta  = 8.0 / 3.0;

//     float dx = sigma * (p.y - p.x);
//     float dy = p.x * (rho - p.z) - p.y;
//     float dz = p.x * p.y - beta * p.z;

//     return vec3(dx, dy, dz) * dt;
// }

#ifdef SPAWN_PARTICLES_PASS
    layout (local_size_x = 8, local_size_y = 8) in;
    void main() {
        int id = global_id;
        set_seed(id);

        if (id == 0) {
            int count = state.particle_count;
            draw_call.index_count = count * 6;
            draw_call.instance_count = 1;
            draw_call.first_index = 0;
            draw_call.vertex_offset = 0;
            draw_call.first_instance = 0;

            state.bad_flag = 0;
        }

        if (id >= ubo.params.spawn_count) {
            return;
        }

        int index = atomicAdd(state.particle_count, 1);
        Particle p;
        p.pos = vec3(random(), random(), random()) * vec3(float(ubo.params.world_size_x), float(ubo.params.world_size_y), float(ubo.params.world_size_z));
        p.vel = 50.0 * (vec3(random(), random(), random()) - 0.5) * 2.0;
        p.type_index = clamp(int(random() * ubo.params.particle_type_count), 0, ubo.params.particle_type_count - 1);
        particles[index] = p;
    }
#endif // SPAWN_PARTICLES_PASS

#ifdef TICK_PARTICLES_PASS
    layout (local_size_x = 8, local_size_y = 8) in;
    void main() {
        int id = global_id;
        set_seed(id);

        if (id >= state.particle_count) {
            return;
        }

        Particle p = particles[id];
        // ParticleType pt = particle_types[p.type_index];

        // TODO: kill and respawn particles using the entropy system
        //  - particles that move less get more exposure or something
        //
        // randomize particles
        {
            if (ubo.params.randomize_particle_types != 0) {
                p.type_index = randuint() % ubo.params.particle_type_count;
                p.age = 0.0;
                p.exposure = 0.0;
            }
            if (ubo.params.randomize_particle_attrs != 0) {
                vec3 world = vec3(float(ubo.params.world_size_x), float(ubo.params.world_size_y), float(ubo.params.world_size_z));
                p.pos = (vec3(random(), random(), random()) - 0.5) * vec3(1000.0);
                p.vel = (vec3(random(), random(), random()) - 0.5) * 2000;
            }
        }

        // ivec3 bpos = ivec3(p.pos / ubo.params.bin_size);
        // ivec3 bworld = ivec3(ubo.params.bin_buf_size_x, ubo.params.bin_buf_size_y, ubo.params.bin_buf_size_z);

        ivec3 world = ivec3(ubo.params.world_size_x, ubo.params.world_size_y, ubo.params.world_size_z);

        vec2 wres = vec2(ubo.frame.width, ubo.frame.height);
        vec3 mouse = vec3(vec2(float(ubo.mouse.x), float(ubo.mouse.y)), 0);
        mouse.xy -= wres / 2.0;
        mouse.xy /= ubo.params.zoom;
        // mouse.xy += wres / 2.0;
        mouse.xy -= ubo.camera.eye.xy;
        mouse.xy += vec2(float(ubo.params.world_size_x), float(ubo.params.world_size_y)) * 0.5;
        // mouse.xy *= ubo.params.zoom;
        // mouse.xy /= ubo.params.grid_size;
        // // mouse.xy /= wres;
        // mouse.xy *= world.xy;

        // p.pos = mouse;
        // p.pos = ubo.camera.eye.xyz / ubo.params.grid_size + vec3(wres.xy/2.0, 0.0);

        vec3 diff = mouse - p.pos;
        diff /= 1000.0;
        vec3 sign = vec3(1.0);
        vec3 offset = vec3(random(), random(), random());
        if (dot(diff, diff) < 0.01 * random()) {
            sign *= -1.0;
        }
        vec3 pforce = 100.0 * normalize(diff)/max(dot(diff, diff), 0.1);
        p.vel *= ubo.params.friction;
        if (ubo.mouse.left == 1) {
            p.vel += sign * offset * pforce * ubo.params.delta;
        }
        if (ubo.mouse.right == 1) {
            p.vel -= offset * pforce * ubo.params.delta;
        }
        p.pos += p.vel * ubo.params.delta;
        // p.vel = normalize(mouse - p.pos) * 100.0;

        // position wrapping
        // p.pos += world * vec3(lessThan(p.pos, vec3(0)));
        // p.pos -= world * vec3(greaterThanEqual(p.pos, world));

        // prevents position blow up
        // p.pos = clamp(p.pos, vec3(0.0), world);

        p.age += 100.0 * ubo.params.delta;
        // TODO: any way to get exposure?
        // p.exposure += _ * ubo.params.delta;

        // if (id != 0) {
        //     p.pos = mouse;
        // }

        f32 scale = 100.0;
        p.pos += attractor(p.pos/scale) * scale;
        particles[id] = p;
    }
#endif // TICK_PARTICLES_PASS

#ifdef RENDER_VERT_PASS
    layout(location = 0) out vec4 vcolor;
    layout(location = 1) out vec2 vuv;
    layout(location = 2) out f32 z_factor;
    void main() {
        int particle_index = gl_VertexIndex / 6;
        int vert_index = gl_VertexIndex % 6;

        Particle p = particles[particle_index];
        ParticleType t = particle_types[p.type_index];
        vec2 vpos = quad_verts[vert_index].xy;

        float zoom = ubo.params.zoom;
        float particle_size = t.particle_scale * ubo.params.particle_visual_size;
        vec2 wres = vec2(ubo.frame.width, ubo.frame.height);

        z_factor = abs(p.pos.z - ubo.params.world_size_z * 0.5) / max(ubo.params.world_size_z * 0.5, 1);
        f32 z_shrink = (1.0 - ubo.params.particle_z_shrinking_factor) + z_factor * ubo.params.particle_z_shrinking_factor;
        z_shrink = clamp(z_shrink, 0, 1);

        // TODO: fix zfactor and zshrink
        z_factor = 0.0;
        // z_shrink = 1.0;

        // TODO: 3d camera rotation
        vec2 pos = p.pos.xy + ubo.camera.eye.xy - vec2(float(ubo.params.world_size_x), float(ubo.params.world_size_y)) * 0.5;
        pos += vpos * 0.5 * particle_size * z_shrink;
        pos /= wres;
        pos *= zoom;
        pos *= 2.0;
        gl_Position = vec4(pos, 0.0, 1.0);

        // vcolor = vec4(0.5, 0.5, 0.5, 1.0);
        vcolor = t.color;
        vuv = quad_uvs[vert_index];
    }
#endif // RENDER_VERT_PASS

#ifdef RENDER_FRAG_PASS
    layout(location = 0) in vec4 vcolor;
    layout(location = 1) in vec2 vuv;
    layout(location = 2) in f32 z_factor;
    layout(location = 0) out vec4 fcolor;
    void main() {
        float zoom = ubo.params.zoom;
        float distanceFromCenter = length(vuv.xy - 0.5);
        float mask = 1.0 - smoothstep(0.5 - z_factor * ubo.params.particle_z_blur_factor - 0.1/zoom, 0.5, distanceFromCenter);
        // mask = pow(1.0 - distanceFromCenter, 4.5) * mask;
        fcolor = vec4(vcolor.xyz, vcolor.a * mask * (0.4 +  0.6 * (1.0 - z_factor)));
    }
#endif // RENDER_FRAG_PASS

#ifdef BG_VERT_PASS
    void main() {
        vec3 pos = quad_verts[gl_VertexIndex];

        pos.z = 1.0 - 0.000001;

        gl_Position = vec4(pos, 1.0);
    }
#endif // BG_VERT_PASS

#ifdef BG_FRAG_PASS
    layout(location = 0) out vec4 fcolor;
    void main() {
        float grid_size = ubo.params.grid_size;
        float zoom = ubo.params.zoom;
        vec2 eye = ubo.camera.eye.xy;
        vec2 wres = vec2(ubo.frame.width, ubo.frame.height);

        vec2 coord = gl_FragCoord.xy;
        coord -= wres / 2.0;
        coord /= zoom;
        coord -= eye;
        coord /= grid_size;

        // TODO: do a starry shader instead of a grid
        vec2 rounded = vec2(floor(coord.x), floor(coord.y));
        float checker = mod(floor(rounded.x) + floor(rounded.y), 2.0);

        vec3 color = mix(vec3(0.01, 0.01, 0.01), vec3(0.05, 0.05, 0.05), checker);

        // debug renderr `particle_bins`
        // ivec2 pos = ivec2(int(coord.x), int(coord.y) + 3);
        // int index = pos.y * ubo.frame.width + pos.x;
        // if (ubo.params.bin_buf_size > index && index >= 0) {
        //     color = vec3(float(particle_bins[index] > ubo.params.particle_count * mod(ubo.frame.time, 1)));
        // }

        // set bad_flag to 1 for debugging
        if (state.bad_flag > 0) {
            color = vec3(1, 0, 0);
        }
        
        fcolor = vec4(color, 1.0);
    }
#endif // BG_FRAG_PASS

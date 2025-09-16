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

        vec2 mres = vec2(ubo.frame.monitor_width, ubo.frame.monitor_height);
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

        ivec3 bpos = ivec3(p.pos / ubo.params.bin_size);
        ivec3 bworld = ivec3(ubo.params.bin_buf_size_x, ubo.params.bin_buf_size_y, ubo.params.bin_buf_size_z);

        ivec3 world = ivec3(ubo.params.world_size_x, ubo.params.world_size_y, ubo.params.world_size_z);

        vec2 wres = vec2(ubo.frame.width, ubo.frame.height);
        vec3 mouse = vec3(2.0 * vec2(float(ubo.mouse.x), float(ubo.mouse.y))/wres - 1.0, 0);
        vec3 pforce = normalize(mouse - p.pos) * 100.0;
        p.vel *= ubo.params.friction;
        p.vel += pforce * ubo.params.delta;
        p.pos += p.vel * ubo.params.delta;

        // position wrapping
        p.pos += world * vec3(lessThan(p.pos, vec3(0)));
        p.pos -= world * vec3(greaterThanEqual(p.pos, world));

        // prevents position blow up
        p.pos = clamp(p.pos, vec3(0.0), world);

        p.age += 100.0 * ubo.params.delta;
        p.exposure += 100.0 * ubo.params.delta;

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
        vec2 mres = vec2(ubo.frame.monitor_width, ubo.frame.monitor_height);
        vec2 wres = vec2(ubo.frame.width, ubo.frame.height);

        z_factor = abs(p.pos.z - ubo.params.world_size_z * 0.5) / max(ubo.params.world_size_z * 0.5, 1);
        f32 z_shrink = (1.0 - ubo.params.particle_z_shrinking_factor) + z_factor * ubo.params.particle_z_shrinking_factor;
        z_shrink = clamp(z_shrink, 0, 1);

        vec2 pos = p.pos.xy + ubo.camera.eye.xy - vec2(float(ubo.params.world_size_x), float(ubo.params.world_size_y)) * 0.5;
        pos += vpos * 0.5 * particle_size * z_shrink;
        pos /= mres; // world space to 0..1
        pos *= mres/wres; // 0..1 scaled wrt window size
        pos *= zoom;
        pos *= 2.0;
        gl_Position = vec4(pos, 0.0, 1.0);

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
        vec2 mres = vec2(ubo.frame.monitor_width, ubo.frame.monitor_height);
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

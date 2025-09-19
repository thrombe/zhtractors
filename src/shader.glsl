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

f32 zoom = 1.0;

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

vec3 thomas_attractor(vec3 pos) {
    float a, b, c, d, e, f;
    float x = pos.x, y = pos.y, z = pos.z;
    float dt = ubo.params.delta;
 
    float dx, dy, dz;

    // a = 0.00 + ubo.frame.time * 0.0009;
    a = 0.13;
    dx = (-a*x + sin(y)) * dt;
    dy = (-a*y + sin(z)) * dt;
    dz = (-a*z + sin(x)) * dt;
    return vec3(dx, dy, dz);
}

vec3 chen_lee_attractor(vec3 pos) {
    float a, b, c, d, e, f;
    float x = pos.x, y = pos.y, z = pos.z;
    float dt = ubo.params.delta;
 
    float dx, dy, dz;

    a = 5.0, b = -10.0;
    c = -0.38, d = 3.0;

    dx = (a*x - y*z) * dt;
    dy = (b*y + x*z) * dt;
    dz = (c*z + (x*y) / d) * dt;
    
    return vec3(dx, dy, dz);
}

vec3 simone_attractor(vec3 pos) {
    float a, b, c, d, e, f;
    float x = pos.x, y = pos.y, z = pos.z;
    float dt = ubo.params.delta;
 
    float dx, dy, dz, xn, yn, zn, scale;
    vec3 next;

    a = 5.51, b = 4.84;
    scale = 3.0;

    xn = sin(a * y) + cos(b * z);
    yn = sin(a * z) + cos(b * x);
    zn = sin(a * x) + cos(b * y);
    next = scale * vec3(xn, yn, zn);
    dx = (next.x - x) * dt;
    dy = (next.y - y) * dt;
    dz = (next.z - z) * dt;

    return vec3(dx, dy, dz);
}

vec3 aizawa_attractor(vec3 pos) {
    float a, b, c, d, e, f;
    float x = pos.x, y = pos.y, z = pos.z;
    float dt = ubo.params.delta;
 
    float dx, dy, dz, xn, yn, zn, scale;
    vec3 next;

    a = 0.80, b = 0.7, c = 0.6;
    d = 3.5, e = 0.1;
    dx = ((z-b) * x - d*y) * dt;
    dy = (d * x + (z-b) * y) * dt;
    dz = (c + a*z - ((z*z*z) / 3.0) - (x*x) + e * z * (x*x*x)) * dt;

    return vec3(dx, dy, dz);
}

// might need a high step count
vec3 dadras_attractor(vec3 pos) {
    float a, b, c, d, e, f;
    float x = pos.x, y = pos.y, z = pos.z;
    float dt = ubo.params.delta;
 
    float dx, dy, dz, xn, yn, zn;
    vec3 next;

    a = 3.0, b = 2.7, c = 1.7;
    d = 2.0, e = 9.0;

    dx = (y- a*x +b*y*z) * dt;
    dy = (c*y -x*z +z) * dt;
    dz = (d*x*y - e*z) * dt;
    
    vec3 dp = vec3(dx, dy, dz);
    return dp;
}

vec3 dequan_li_attractor(vec3 pos) {
    f32 scale = 0.04;
    pos = pos / scale;

    float a, b, c, d, e, f;
    float x = pos.x, y = pos.y, z = pos.z;
    float dt = ubo.params.delta / 5.0;
 
    float dx, dy, dz, xn, yn, zn;
    vec3 next;

    a = 40.0, b = 1.833, c = 0.16;
    d = 0.65, e = 55.0, f = 20.0;
    dx = (a*(y-x) + c*x*z) * dt;
    dy = (e*x + f*y - x*z) * dt;
    dz = (b*z + x*y - d*x*x) * dt;
    vec3 dp = vec3(dx, dy, dz);
    dp = dp * 0.001;

    return dp;
}

vec3 arneodo_attractor(vec3 pos) {
    float a, b, c, d, e, f;
    float x = pos.x, y = pos.y, z = pos.z;
    float dt = ubo.params.delta;
 
    float dx, dy, dz, xn, yn, zn;
    vec3 next;

    a = -5.5, b = 3.5, d = -1.0;
    dx = y * dt;
    dy = z * dt;
    dz = (-a*x -b*y -z + (d * (pow(x, 3.0)))) * dt;
    vec3 dp = vec3(dx, dy, dz);

    return dp;
}

vec3 three_scroll_attractor(vec3 pos) {
    f32 scale = 0.06;
    pos = pos / scale;

    float a, b, c, d, e, f;
    float x = pos.x, y = pos.y, z = pos.z;
    float dt = ubo.params.delta * 0.4;
 
    float dx, dy, dz, xn, yn, zn;
    vec3 next;

    a = 40., b = 0.833, c = 0.5;
    d = 0.65, e = 20.0;

    dx = (a * ( y - x ) + c * x * z) * dt;
    dy = (e * y - x * z) * dt;
    dz = (b * z + x * y - d * x * x) * dt;

    vec3 dp = vec3(dx, dy, dz);
    dp = dp * 0.03;

    return dp;
}

// https://blog.shashanktomar.com/posts/strange-attractors
vec3 attractor(vec3 pos) {
    return thomas_attractor(pos);
    // return aizawa_attractor(pos);
    // return chen_lee_attractor(pos);
    // return simone_attractor(pos);
    // return arneodo_attractor(pos);
    // return dequan_li_attractor(pos);
    // vec3 a1 = thomas_attractor(pos);
    // return a1;
    // vec3 a2 = chen_lee_attractor(pos);
    // return a2;
    // a1 = normalize(a1);
    // a2 = normalize(a2);
    // a1 *= 0.01;
    // a2 *= 0.01;
    // return max(abs(a1), abs(a2)) * sign(a1);
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

        int index = atomicAdd(state.particle_count, 1);
        Particle p;
        p.pos = vec3(random(), random(), random()) * 1200;
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
            u32 killed = 0;

            f32 vel = length(p.vel);
            f32 entropy = 0.0;
            entropy += float(length(p.pos) > 10000.0) * 100.01;
            entropy += float(vel < 10.0) * 0.001 + float(vel > 500.0) * 0.001;
            entropy += sqrt(p.exposure) * 0.000001;
            entropy += float(p.age > 1000.0) * 0.000003;
            entropy *= ubo.params.entropy;

            // framerate and step independent entropy
            entropy *= ubo.params.delta * 100.0;

            if (random() < entropy) {
                killed = 1;
            }

            if (ubo.params.randomize_particle_types != 0 || killed == 1) {
                p.type_index = randuint() % ubo.params.particle_type_count;
                p.age = 0.0;
                p.exposure = 0.0;
            }
            if (ubo.params.randomize_particle_attrs != 0 || killed == 1) {
                p.scale = random();
                p.pos = (vec3(random(), random(), random()) - 0.5) * vec3(1000.0);
                p.vel = (vec3(random(), random(), random()) - 0.5) * 2000;
            }
        }

        // position of the particle from a ray starting 100 units in front of cam
        vec3 v = p.pos - (ubo.camera.eye + ubo.camera.fwd * 100.0);
        f32 t = dot(v, ubo.camera.fwd);
        vec3 proj = t * ubo.camera.fwd;
        vec3 rej = v - proj;
        f32 diff;
        if (t < 0) {
            diff = length(v);
        } else {
            diff = length(rej);
        }

        diff /= 40.0;
        vec3 sign = vec3(1.0);
        vec3 offset = vec3(random(), random(), random());
        if (diff < 0.01 * random()) {
            sign *= -1.0;
        }
        vec3 pforce = - ubo.params.attraction_strength_scale * rej/max(diff * diff, 0.1);
        p.vel *= ubo.params.friction;
        if (ubo.mouse.left == 1) {
            p.vel += sign * offset * pforce * ubo.params.delta;
        }
        if (ubo.mouse.right == 1) {
            p.vel -= offset * pforce * ubo.params.delta;
        }

        f32 scale = 100.0;
        vec3 dp = attractor(p.pos/scale) * scale;
        // p.pos += dp;
        p.vel += (dp / max(ubo.params.delta, 0.0001)) * (1.0 - exp(-ubo.params.delta * ubo.params.attractor_inertia));

        p.pos += p.vel * ubo.params.delta;

        // position wrapping
        // p.pos += world * vec3(lessThan(p.pos, vec3(0)));
        // p.pos -= world * vec3(greaterThanEqual(p.pos, world));

        // prevents position blow up
        // p.pos = clamp(p.pos, vec3(0.0), world);

        p.age += 100.0 * ubo.params.delta;
        p.exposure += (1.0/max(length(dp), 0.001)) * 1.0 * ubo.params.delta;

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

        float particle_size = t.particle_scale * ubo.params.particle_visual_size;
        vpos = vpos * particle_size * (p.scale + 0.4);
        vec4 pos = ubo.params.world_to_screen * vec4(p.pos + ubo.camera.right * vpos.x + ubo.camera.up * vpos.y, 1.0);

        z_factor = clamp(pos.z / pos.w - 0.8, 0.0, 1.0);
        gl_Position = pos;

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
        vec2 wres = vec2(ubo.frame.width, ubo.frame.height);

        vec2 coord = gl_FragCoord.xy;
        coord -= wres / 2.0;
        coord /= zoom;
        coord /= grid_size;

        // TODO: do a starry shader instead of a grid
        vec2 rounded = vec2(floor(coord.x), floor(coord.y));
        float checker = mod(floor(rounded.x) + floor(rounded.y), 2.0);

        vec3 color = mix(vec3(0.01, 0.01, 0.01), vec3(0.05, 0.05, 0.05), checker);

        f32 dcen = length(gl_FragCoord.xy - wres/2.0);
        dcen /= 3.0;
        if (dcen < 1.0) {
            color = mix(color, vec3(0.6), 1.0 - smoothstep(0.6, 1, dcen));
        }

        // set bad_flag to 1 for debugging
        if (state.bad_flag > 0) {
            color = vec3(1, 0, 0);
        }
        
        fcolor = vec4(color, 1.0);
    }
#endif // BG_FRAG_PASS

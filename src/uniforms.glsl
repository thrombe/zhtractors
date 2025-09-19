 // This file is generated from code. DO NOT EDIT.

 struct DrawCall {
     uint index_count;
     uint instance_count;
     uint first_index;
     int vertex_offset;
     uint first_instance;
 };

 struct ParticleType {
     vec4 color;
     float particle_scale;
 };

 struct Particle {
     vec3 pos;
     vec3 vel;
     float scale;
     uint type_index;
     float age;
     float exposure;
 };

 struct Params {
     mat4 world_to_screen;
     float delta;
     uint steps_per_frame;
     float particle_visual_size;
     uint grid_size;
     float particle_z_blur_factor;
     float attractor_inertia;
     float friction;
     float entropy;
     float attraction_strength_scale;
     uint randomize_particle_types;
     uint randomize_particle_attrs;
     uint particle_type_count;
     uint particle_count;
     uint spawn_count;
 };

 struct PushConstants {
     int seed;
 };

 struct Camera3DMeta {
     uint did_change;
     uint did_move;
     uint did_rotate;
 };

 struct Camera3D {
     vec3 eye;
     vec3 fwd;
     vec3 right;
     vec3 up;
     Camera3DMeta meta;
 };

 struct Frame {
     uint frame;
     float time;
     float deltatime;
     int width;
     int height;
 };

 struct Mouse {
     int x;
     int y;
     uint left;
     uint right;
     uint middle;
 };

 struct Uniforms {
     Camera3D camera;
     Frame frame;
     Mouse mouse;
     Params params;
 };

 const int _bind_camera = 0;
 const int _bind_particles_draw_call = 1;
 const int _bind_scratch = 2;
 const int _bind_particle_types = 3;
 const int _bind_particles = 4;


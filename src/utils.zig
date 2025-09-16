const std = @import("std");
const builtin = @import("builtin");

const math = @import("math.zig");

const main = @import("main.zig");
const allocator = main.allocator;

// - [typeId in zig?](https://github.com/ziglang/zig/issues/19858#issuecomment-2596933195)
pub const TypeId = extern struct {
    // this type is marked extern because comptime TypeId that use hashing complain about 'well-defined layout'
    // - [ensure comptime TypeId s](https://github.com/ziglang/zig/issues/425#issuecomment-754572088)

    id: u64,

    // unique even across dylib loads. (loading same lib multiple times will create different ids)
    pub inline fn unique(comptime T: type) @This() {
        const Phantom = *const struct {
            _: u8,
        };
        const ptr = &struct {
            comptime {
                _ = T;
            }
            var id: @typeInfo(Phantom).pointer.child = undefined;
        }.id;

        return .{ .id = @intCast(@intFromPtr(ptr)) };
    }

    pub inline fn from_name(comptime T: type) @This() {
        comptime {
            var hasher = std.hash.Wyhash.init(0);
            hasher.update(@typeName(T));
            return .{ .id = hasher.final() };
        }
    }

    pub inline fn from_type(comptime T: type) @This() {
        comptime {
            var hasher = std.hash.Wyhash.init(0);

            const name_hash = from_name(T);
            hasher.update(std.mem.asBytes(&name_hash.id));
            const type_hash = _from_type(T);
            hasher.update(std.mem.asBytes(&type_hash.id));

            return .{ .id = hasher.final() };
        }
    }

    pub inline fn _from_type(comptime T: type) @This() {
        comptime {
            @setEvalBranchQuota(10000);
            const Ti = @typeInfo(T);

            var hasher = std.hash.Wyhash.init(0);

            // OOF: parallel compilation strikes again.
            // anon structs have different names compilation to compilation
            // hasher.update(@typeName(T));

            hasher.update(@tagName(std.meta.activeTag(Ti)));

            switch (T) {
                std.mem.Allocator => return .{ .id = hasher.final() },
                else => {},
            }

            switch (Ti) {
                .@"struct" => |t| {
                    for (t.fields) |field| {
                        hasher.update(field.name);
                        hasher.update(std.mem.asBytes(&@as(u32, field.alignment)));
                        hasher.update(std.mem.asBytes(&TypeId._from_type(field.type)));
                    }
                },
                .int => |t| {
                    hasher.update(@tagName(t.signedness));
                    hasher.update(std.mem.asBytes(&t.bits));
                },
                .float => |t| {
                    hasher.update(std.mem.asBytes(&t.bits));
                },
                .@"enum" => |t| {
                    for (t.fields) |field| {
                        hasher.update(field.name);
                        hasher.update(std.mem.asBytes(&@as(u32, field.value)));
                    }
                },
                .@"union" => |t| {
                    hasher.update(@tagName(t.layout));
                    for (t.fields) |field| {
                        hasher.update(field.name);
                        hasher.update(std.mem.asBytes(&TypeId._from_type(field.type)));
                        hasher.update(std.mem.asBytes(&@as(u32, field.alignment)));
                    }
                },
                .optional => |t| {
                    hasher.update(std.mem.asBytes(&TypeId._from_type(t.child)));
                },
                .pointer => |t| {
                    // not sure what to do here :/
                    // might have cycles
                    hasher.update(@tagName(t.size));
                    hasher.update(@tagName(t.address_space));

                    // OOF: ?this produces different hash for the same type??
                    // hasher.update(std.mem.asBytes(&@as(u32, t.alignment)));

                    hasher.update(std.mem.asBytes(&TypeId._from_type(t.child)));
                },
                .array => |t| {
                    hasher.update(std.mem.asBytes(&@as(u32, t.len)));
                    hasher.update(std.mem.asBytes(&TypeId._from_type(t.child)));
                },
                .bool, .void, .@"opaque" => {},
                else => @compileLog("can't do TypeIds on this type yet: " ++ @typeName(T)),
            }

            return .{ .id = hasher.final() };
        }
    }
};

pub inline fn cast(typ: type, val: anytype) typ {
    const E = @typeInfo(@TypeOf(val));
    const T = @typeInfo(typ);
    if (comptime (std.meta.activeTag(E) == .int and std.meta.activeTag(T) == .float)) {
        return @floatFromInt(val);
    }
    if (comptime (std.meta.activeTag(E) == .float and std.meta.activeTag(T) == .int)) {
        return @intFromFloat(val);
    }
    if (comptime (std.meta.activeTag(E) == .float and std.meta.activeTag(T) == .float)) {
        return @floatCast(val);
    }
    if (comptime (std.meta.activeTag(E) == .int and std.meta.activeTag(T) == .int)) {
        return @intCast(val);
    }
    if (comptime (std.meta.activeTag(E) == .comptime_int)) {
        return val;
    }
    if (comptime (std.meta.activeTag(E) == .comptime_float)) {
        if (comptime (std.meta.activeTag(T) == .float)) {
            return val;
        }
        if (comptime (std.meta.activeTag(T) == .int)) {
            return @intFromFloat(@as(f32, val));
        }
    }
    @compileError("can't cast from '" ++ @typeName(@TypeOf(val)) ++ "' to '" ++ @typeName(typ) ++ "'");
}

pub inline fn tuple_union(a: type, b: type) type {
    comptime {
        const fields_a = std.meta.fields(a);
        const fields_b = std.meta.fields(b);
        var fields: [fields_a.len + fields_b.len]std.builtin.Type.StructField = undefined;
        for (fields_a, 0..) |field, i| {
            fields[i] = field;
            fields[i].name = std.fmt.comptimePrint("{d}", .{i});
        }
        for (fields_b, fields_a.len..) |*field, i| {
            fields[i] = field;
            fields[i].name = std.fmt.comptimePrint("{d}", .{i});
        }
        return @Type(.{ .@"struct" = .{
            .layout = .auto,
            .fields = &fields,
            .decls = &.{},
            .is_tuple = true,
        } });
    }
}

pub inline fn indexof_type(types: []const type, typ: type) ?usize {
    comptime {
        for (types, 0..) |t, i| {
            if (t == typ) return i;
        }
        return null;
    }
}

pub inline fn type_array_from_struct_decls(typ: type) [@typeInfo(typ).@"struct".decls.len]type {
    comptime {
        var types: [@typeInfo(typ).@"struct".decls.len]type = undefined;
        for (@typeInfo(typ).@"struct".decls, 0..) |decl, i| {
            types[i] = @field(typ, decl.name);
        }
        return types;
    }
}

pub inline fn dump_error(err: anyerror) void {
    std.debug.print("error: {any}\n", .{err});
    if (builtin.os.tag != .windows) {
        if (@errorReturnTrace()) |trace| {
            std.debug.dumpStackTrace(trace.*);
        }
    }
}

pub inline fn deinit_fields(self: anytype) void {
    inline for (@typeInfo(@This()).@"struct".fields) |field| {
        if (comptime @hasDecl(field.type, "deinit")) {
            @field(self, field.name).deinit();
        }
    }
}

pub const fspath = struct {
    inline fn len(comptime parts: anytype) usize {
        comptime {
            var total_len: usize = 0;
            for (parts, 0..) |part, i| {
                if (i > 0) {
                    total_len += 1;
                }
                total_len += part.len;
            }
            return total_len;
        }
    }

    pub inline fn join(comptime parts: anytype) [len(parts)]u8 {
        comptime {
            const total_len = len(parts);
            var buffer: [total_len]u8 = undefined;
            var index: usize = 0;
            for (parts, 0..) |part, i| {
                if (i > 0) {
                    buffer[index] = std.fs.path.sep;
                    index += 1;
                }

                @memcpy(buffer[index..][0..part.len], part);
                index += part.len;
            }

            return buffer;
        }
    }

    pub fn cwd_join(alloc: std.mem.Allocator, path: []const u8) ![]const u8 {
        var buf: [std.fs.max_path_bytes]u8 = undefined;
        const pwd = try std.posix.getcwd(&buf);
        return try std.fs.path.join(alloc, &[_][]const u8{ pwd, path });
    }

    pub fn replace_sep(path: []const u8) ![]const u8 {
        return try std.mem.replaceOwned(
            u8,
            allocator.*,
            path,
            std.fs.path.sep_str_posix,
            std.fs.path.sep_str,
        );
    }
};

pub const SimulationTicker = struct {
    speed: struct {
        perc: f32 = 1.0,
        numerator: u64 = 1,
        denominator: u64 = 1,
    } = .{},

    real: struct {
        timer: std.time.Timer,

        // each frame we update these. lap is delta but in ns
        delta: f32 = 0,
        lap: u64 = 0,

        time_ns: u64 = 0,
        time_f: f32 = 0,
    },

    // real data scaled by speed
    scaled: struct {
        delta: f32 = 0,
        lap: u64 = 0,

        time_ns: u64 = 0,
        time_f: f32 = 0,
    } = .{},

    simulation: struct {
        steps_per_sec: u32 = 60,
        step_f: f32 = 1.0 / 60.0,
        step_ns: u64 = std.time.ns_per_s / 60,

        acctime_ns: u64 = 0,
        acctime_f: f32 = 0,
        interpolation_factor: f32 = 0,

        time_ns: u64 = 0,
        time_f: f32 = 0,

        lap: u64 = 0,
        delta: f32 = 0,

        ticks: struct {
            pending: u64 = 0,
            must_step: bool = false,
        } = .{},
    } = .{},

    animation: struct {
        delta: f32 = 0,
        lap: u64 = 0,

        time_ns: u64 = 0,
        time_f: f32 = 0,
    } = .{},

    fn ns_to_s(t: u64) f32 {
        return @floatCast(@as(f64, @floatFromInt(t)) / @as(f64, @floatFromInt(std.time.ns_per_s)));
    }

    pub fn init() !@This() {
        return .{
            .real = .{
                .timer = try std.time.Timer.start(),
            },
        };
    }

    pub fn set_steps_per_sec(self: *@This(), num: u32) void {
        self.simulation.steps_per_sec = num;
        self.simulation.step_f = 1.0 / @as(f32, @floatFromInt(num));
        self.simulation.step_ns = std.time.ns_per_s / @as(u64, @intCast(num));
    }

    pub fn tick_real(self: *@This()) void {
        self.real.lap = self.real.timer.lap();
        self.real.delta = ns_to_s(self.real.lap);
        self.real.time_ns += self.real.lap;
        self.real.time_f = ns_to_s(self.real.time_ns);

        self.scaled.lap = cast(u64, (cast(u128, self.real.lap) * self.speed.numerator) / self.speed.denominator);
        self.scaled.delta = ns_to_s(self.scaled.lap);
        self.scaled.time_ns += self.scaled.lap;
        self.scaled.time_f = ns_to_s(self.scaled.time_ns);

        self.simulation.acctime_ns += self.scaled.lap;
    }

    pub fn tick_simulation(self: *@This()) bool {
        self.simulation.ticks.pending = self.simulation.acctime_ns / self.simulation.step_ns;
        if (self.simulation.ticks.pending > 0) {
            self.simulation.ticks.must_step = true;
            self.simulation.ticks.pending -= 1;
            self.simulation.acctime_ns -= self.simulation.step_ns;
        } else {
            self.simulation.ticks.must_step = false;
        }
        self.simulation.acctime_f = ns_to_s(self.simulation.acctime_ns);
        self.simulation.interpolation_factor = @min(self.simulation.acctime_f / self.simulation.step_f, 1);

        if (self.simulation.ticks.must_step) {
            self.simulation.lap = self.simulation.step_ns;
        } else {
            self.simulation.lap = 0;
        }
        self.simulation.delta = ns_to_s(self.simulation.lap);
        self.simulation.time_ns += self.simulation.lap;
        self.simulation.time_f = ns_to_s(self.simulation.time_ns);

        return self.simulation.ticks.must_step;
    }

    pub fn tick_animation(self: *@This()) void {
        const anim_time = self.animation.time_ns;
        // TODO: this condition is broken
        // keep animation flowing smoothly as long as physics isn't lagging
        if (false and self.simulation.ticks.pending == 0) {
            self.animation.time_ns = self.scaled.time_ns;

            // (optional) physics is interpolated each frame from last to current step's transform
            // we can try to keep animation and interpolated simulation in sync
            self.animation.time_ns -= self.simulation.step_ns;
        } else {
            self.animation.time_ns = self.simulation.time_ns;
        }
        self.animation.lap = self.animation.time_ns - anim_time;
        self.animation.delta = ns_to_s(self.animation.lap);
        self.animation.time_f = ns_to_s(self.animation.time_ns);
    }

    pub fn set_speed(self: *@This(), perc: f64) void {
        self.speed.perc = @floatCast(perc);
        self.speed.numerator = @intFromFloat(std.time.ns_per_s * perc);
        self.speed.denominator = std.time.ns_per_s;

        const gcd = std.math.gcd(self.speed.numerator, self.speed.denominator);
        self.speed.numerator /= gcd;
        self.speed.denominator /= gcd;
    }

    pub fn drop_pending_simtime(self: *@This()) void {
        // if simulation is lagging, this function will drop the extra time
        if (self.simulation.ticks.pending > 0) {
            self.simulation.acctime_ns = 0;
            self.scaled.time_ns = self.simulation.time_ns;
        }
    }

    pub fn reset(self: *@This()) void {
        self.real.timer.reset();
        self.* = .{
            .speed = self.speed,
            .real = .{ .timer = self.real.timer },
            .simulation = .{
                .step_ns = self.simulation.step_ns,
                .step_f = self.simulation.step_f,
            },
        };
    }
};

pub const Ticker = struct {
    last: u64 = 0,
    step: u64,

    pub fn init(step: u64) @This() {
        return .{
            .step = step,
            .last = 0,
        };
    }

    pub fn tick(self: *@This(), time: u64) bool {
        if (time - self.last >= self.step) {
            self.last = time;
            return true;
        } else {
            return false;
        }
    }
};

// assumes ok has ok.deinit()
pub fn Result(ok: type, err_typ: type) type {
    return union(enum) {
        Ok: ok,
        Err: Error,

        pub const Error = struct {
            err: err_typ,
            msg: []u8,

            // pub fn owned(err: err_typ, msg: []const u8) !@This() {
            //     return .{
            //         .err = err,
            //         .msg = try allocator.dupe(u8, msg),
            //     };
            // }
            // pub fn deinit(self: *@This()) void {
            //     allocator.free(self.msg);
            // }
        };

        // pub fn deinit(self: *@This()) void {
        //     switch (self) {
        //         .Ok => |res| {
        //             if (std.meta.hasMethod(ok, "deinit")) {
        //                 res.deinit();
        //             }
        //         },
        //         .Err => |err| {
        //             err.deinit();
        //         },
        //     }
        // }
    };
}

pub fn Deque(typ: type) type {
    return struct {
        allocator: std.mem.Allocator,
        buffer: []typ,
        size: usize,

        // fill this index next
        front: usize, // at
        back: usize, // one to the right

        pub fn init(alloc: std.mem.Allocator) !@This() {
            const len = 32;
            const buffer = try alloc.alloc(typ, len);
            return .{
                .allocator = alloc,
                .buffer = buffer,
                .front = 0,
                .back = 0,
                .size = 0,
            };
        }

        pub fn deinit(self: *@This()) void {
            self.allocator.free(self.buffer);
        }

        pub fn push_front(self: *@This(), value: typ) !void {
            if (self.size == self.buffer.len) {
                try self.resize();
                return self.push_front(value) catch unreachable;
            }
            self.front = (self.front + self.buffer.len - 1) % self.buffer.len;
            self.buffer[self.front] = value;
            self.size += 1;
        }

        pub fn push_back(self: *@This(), value: typ) !void {
            if (self.size == self.buffer.len) {
                try self.resize();
                return self.push_back(value) catch unreachable;
            }
            self.buffer[self.back] = value;
            self.back = (self.back + 1) % self.buffer.len;
            self.size += 1;
        }

        pub fn pop_front(self: *@This()) ?typ {
            if (self.size == 0) {
                return null;
            }
            const value = self.buffer[self.front];
            self.front = (self.front + 1) % self.buffer.len;
            self.size -= 1;
            return value;
        }

        pub fn pop_back(self: *@This()) ?typ {
            if (self.size == 0) {
                return null;
            }
            self.back = (self.back + self.buffer.len - 1) % self.buffer.len;
            const value = self.buffer[self.back];
            self.size -= 1;
            return value;
        }

        pub fn peek_front(self: *@This()) ?*const typ {
            if (self.size == 0) {
                return null;
            }
            return &self.buffer[self.front];
        }

        pub fn peek_back(self: *@This()) ?*const typ {
            if (self.size == 0) {
                return null;
            }
            const back = (self.back + self.buffer.len - 1) % self.buffer.len;
            return &self.buffer[back];
        }

        pub fn is_empty(self: *@This()) bool {
            return self.size == 0;
        }

        fn resize(self: *@This()) !void {
            std.debug.assert(self.size == self.buffer.len);

            const size = self.buffer.len * 2;
            const buffer = try self.allocator.alloc(typ, size);
            @memcpy(buffer[0 .. self.size - self.front], self.buffer[self.front..]);
            @memcpy(buffer[self.size - self.front .. self.size], self.buffer[0..self.front]);
            const new = @This(){
                .allocator = self.allocator,
                .buffer = buffer,
                .front = 0,
                .back = self.size,
                .size = self.size,
            };
            self.allocator.free(self.buffer);
            self.* = new;
        }
    };
}

// MAYBE: condvars + .block_recv()
pub fn Channel(typ: type) type {
    return struct {
        const Dq = Deque(typ);
        const Pinned = struct {
            dq: Dq,
            lock: std.Thread.Mutex = .{},
        };
        pinned: *Pinned,

        pub fn init(alloc: std.mem.Allocator) !@This() {
            const dq = try Dq.init(alloc);
            const pinned = try alloc.create(Pinned);
            pinned.* = .{
                .dq = dq,
            };
            return .{
                .pinned = pinned,
            };
        }

        pub fn deinit(self: *@This()) void {
            self.pinned.lock.lock();
            // defer self.pinned.lock.unlock();
            self.pinned.dq.deinit();
            self.pinned.dq.allocator.destroy(self.pinned);
        }

        pub fn send(self: *@This(), val: typ) !void {
            self.pinned.lock.lock();
            defer self.pinned.lock.unlock();
            try self.pinned.dq.push_back(val);
        }

        pub fn try_recv(self: *@This()) ?typ {
            self.pinned.lock.lock();
            defer self.pinned.lock.unlock();
            return self.pinned.dq.pop_front();
        }

        pub fn can_recv(self: *@This()) bool {
            self.pinned.lock.lock();
            defer self.pinned.lock.unlock();
            return self.pinned.dq.peek_front() != null;
        }
    };
}

pub const JsonHelpers = struct {
    pub fn parseUnionAsStringWithUnknown(t: type, alloc: std.mem.Allocator, source: anytype, options: std.json.ParseOptions) !t {
        switch (try source.nextAlloc(alloc, options.allocate orelse .alloc_if_needed)) {
            .string, .allocated_string => |field| {
                inline for (@typeInfo(t).@"union".fields) |typ| {
                    comptime if (std.meta.stringToEnum(std.meta.Tag(t), typ.name).? == .unknown) {
                        continue;
                    };

                    if (std.mem.eql(u8, field, typ.name)) {
                        return @unionInit(t, typ.name, {});
                    }
                }
                return .{ .unknown = field };
            },
            else => return error.UnexpectedToken,
        }
    }
};

pub const Fuse = struct {
    fused: std.atomic.Value(bool) = .{ .raw = false },

    pub fn fuse(self: *@This()) bool {
        return self.fused.swap(true, .release);
    }
    pub fn unfuse(self: *@This()) bool {
        const res = self.fused.swap(false, .release);
        return res;
    }
    pub fn check(self: *@This()) bool {
        return self.fused.load(.acquire);
    }
};

pub const FsFuse = struct {
    // - [emcrisostomo/fswatch](https://github.com/emcrisostomo/fswatch?tab=readme-ov-file#libfswatch)
    // - [libfswatch/c/libfswatch.h Reference](http://emcrisostomo.github.io/fswatch/doc/1.17.1/libfswatch.html/libfswatch_8h.html#ae465ef0618fb1dc6d8b70dee68359ea6)
    const c = @cImport({
        @cInclude("libfswatch/c/libfswatch.h");
    });

    const Event = struct {
        file: []const u8,
        real: []const u8,

        pub fn deinit(self: *const @This()) void {
            allocator.free(self.file);
            allocator.free(self.real);
        }
    };
    const Chan = Channel(Event);
    const Ctx = struct {
        handle: c.FSW_HANDLE,
        channel: Chan,
        path: [:0]const u8,
    };

    ctx: *Ctx,
    thread: std.Thread,

    pub fn init(path: [:0]const u8) !@This() {
        const pathZ = try allocator.dupeZ(u8, path);

        const watch = try start(pathZ);
        return watch;
    }

    pub fn deinit(self: @This()) void {
        _ = c.fsw_stop_monitor(self.ctx.handle);
        _ = c.fsw_destroy_session(self.ctx.handle);

        // OOF: freezes the thread for a while
        // self.thread.join();

        self.ctx.channel.deinit();
        allocator.free(self.ctx.path);
        allocator.destroy(self.ctx);
    }

    pub fn can_recv(self: *@This()) bool {
        return self.ctx.channel.can_recv();
    }

    pub fn try_recv(self: *@This()) ?Event {
        return self.ctx.channel.try_recv();
    }

    pub fn restart(self: *@This(), path: [:0]const u8) !void {
        self.deinit();
        self.* = try init(path);
    }

    fn start(path: [:0]const u8) !@This() {
        const ok = c.fsw_init_library();
        if (ok != c.FSW_OK) {
            return error.CouldNotCreateFsWatcher;
        }

        const ctxt = try allocator.create(Ctx);
        ctxt.* = .{
            .channel = try Chan.init(allocator.*),
            .handle = null,
            .path = path,
        };

        const Callbacks = struct {
            fn spawn(ctx: *Ctx) !void {
                ctx.handle = c.fsw_init_session(c.filter_include) orelse return error.CouldNotInitFsWatcher;
                var oke = c.fsw_add_path(ctx.handle, ctx.path.ptr);
                if (oke != c.FSW_OK) {
                    return error.PathAdditionFailed;
                }

                oke = c.fsw_set_recursive(ctx.handle, true);
                if (oke != c.FSW_OK) {
                    return error.FswSetFailed;
                }
                oke = c.fsw_set_latency(ctx.handle, 0.2);
                if (oke != c.FSW_OK) {
                    return error.FswSetFailed;
                }
                oke = c.fsw_set_callback(ctx.handle, @ptrCast(&event_callback), ctx);
                if (oke != c.FSW_OK) {
                    return error.FswSetFailed;
                }

                std.debug.print("starting monitor\n", .{});
                oke = c.fsw_start_monitor(ctx.handle);
                if (oke != c.FSW_OK) {
                    return error.CouldNotStartWatcher;
                }
            }
            fn event_callback(events: [*c]const c.fsw_cevent, num: c_uint, ctx: ?*Ctx) callconv(.C) void {
                var flags = c.NoOp;
                // flags |= c.Created;
                flags |= c.Updated;
                // flags |= c.Removed;
                // flags |= c.Renamed;
                // flags |= c.OwnerModified;
                // flags |= c.AttributeModified;
                // flags |= c.MovedFrom;
                flags |= c.MovedTo;
                // flags |= c.CloseWrite;

                for (events[0..@intCast(num)]) |event| {
                    for (event.flags[0..event.flags_num]) |f| {
                        if (flags & @as(c_int, @intCast(f)) == 0) {
                            continue;
                        }
                        const name = c.fsw_get_event_flag_name(f);
                        // std.debug.print("Path: {s}\n", .{event.path});
                        // std.debug.print("Event Type: {s}\n", .{std.mem.span(name)});

                        if (ctx) |cctx| {
                            const stripped = std.fs.path.relative(allocator.*, cctx.path, std.mem.span(event.path)) catch unreachable;
                            cctx.channel.send(.{
                                .file = stripped,
                                .real = allocator.dupe(u8, std.mem.span(event.path)) catch unreachable,
                            }) catch unreachable;
                        } else {
                            std.debug.print("Error: Event ignored! type: '{s}' path: '{s}'", .{ event.path, name });
                        }
                    }
                }
            }
        };

        const t = try std.Thread.spawn(.{ .allocator = allocator.* }, Callbacks.spawn, .{ctxt});
        return .{
            .ctx = ctxt,
            .thread = t,
        };
    }
};

pub const StbImage = struct {
    const c = @cImport({
        @cInclude("stb_image.h");
        @cInclude("stb_image_write.h");
    });

    pub const PixelType = enum {
        unorm,
        half,
        float,

        fn typ(comptime self: @This()) type {
            return switch (self) {
                .unorm => u8,
                .half => f16,
                .float => f32,
            };
        }

        fn img_typ(comptime self: @This()) type {
            return Image(Pixel(self.typ()));
        }
    };
    pub fn Pixel(typ: type) type {
        return extern struct {
            r: typ,
            g: typ,
            b: typ,
            a: typ,
        };
    }
    pub fn Image(pix: type) type {
        return struct {
            buffer: []pix,
            height: usize,
            width: usize,

            pub fn deinit(self: *@This()) void {
                allocator.free(self.buffer);
            }
        };
    }

    pub const FloatImage = PixelType.float.img_typ();
    pub const HalfImage = PixelType.half.img_typ();
    pub const UnormImage = PixelType.unorm.img_typ();

    pub fn from_file(comptime typ: PixelType, _path: []const u8) !Image(Pixel(typ.typ())) {
        const path = try fspath.cwd_join(allocator.*, _path);
        defer allocator.free(path);
        var file = try std.fs.openFileAbsolute(path, .{});
        defer file.close();

        const buf = try file.readToEndAlloc(allocator.*, 50 * 1000 * 1000);
        defer allocator.free(buf);

        return decode_img(typ, buf);
    }

    pub fn decode_img(comptime typ: PixelType, bytes: []u8) !Image(Pixel(typ.typ())) {
        var img_width: i32 = 0;
        var img_height: i32 = 0;
        var channels: i32 = 0;
        const img_data = c.stbi_load_from_memory(
            bytes.ptr,
            @intCast(bytes.len),
            &img_width,
            &img_height,
            &channels,
            4,
        );
        defer c.stbi_image_free(img_data);

        const img = std.mem.bytesAsSlice(Pixel(u8), img_data[0..@intCast(img_width * img_height * 4)]);

        switch (typ) {
            .half, .float => {
                const buf = try allocator.alloc(Pixel(typ.typ()), @intCast(img_width * img_height));
                for (img, 0..) |v, i| {
                    buf[i] = .{
                        .r = @as(typ.typ(), @floatFromInt(v.r)) / 255.0,
                        .g = @as(typ.typ(), @floatFromInt(v.g)) / 255.0,
                        .b = @as(typ.typ(), @floatFromInt(v.b)) / 255.0,
                        .a = @as(typ.typ(), @floatFromInt(v.a)) / 255.0,
                    };
                }
                return .{
                    .buffer = buf,
                    .height = @intCast(img_height),
                    .width = @intCast(img_width),
                };
            },
            .unorm => {
                return .{
                    .buffer = try allocator.dupe(Pixel(u8), img),
                    .height = @intCast(img_height),
                    .width = @intCast(img_width),
                };
            },
        }
    }

    pub fn encode_rgba_image(pixels: []u8, width: usize, height: usize) ![]u8 {
        const Ctx = struct {
            buf: std.ArrayList(u8),
            err: ?anyerror = null,

            fn write(ctx_ptr: ?*anyopaque, opaque_data: ?*anyopaque, size: i32) callconv(.c) void {
                const data: [*c]u8 = @ptrCast(opaque_data.?);
                const ctx: *@This() = @ptrCast(@alignCast(ctx_ptr.?));

                if (ctx.err) |_| {
                    return;
                }

                ctx.buf.appendSlice(data[0..@as(usize, @intCast(size))]) catch |e| {
                    ctx.err = e;
                };
            }
        };

        var context = Ctx{
            .buf = .init(allocator.*),
        };
        errdefer context.buf.deinit();
        if (c.stbi_write_png_to_func(
            &Ctx.write,
            @ptrCast(&context),
            @intCast(width),
            @intCast(height),
            4,
            pixels.ptr,
            @intCast(width * 4),
        ) == 0) {
            return error.FailedWritingPng;
        }

        if (context.err) |e| {
            return e;
        }

        return try context.buf.toOwnedSlice();
    }
};

pub const ImageMagick = struct {
    // - [ImageMagick – Sitemap](https://imagemagick.org/script/sitemap.php#program-interfaces)
    // - [ImageMagick – MagickWand, C API](https://imagemagick.org/script/magick-wand.php)
    // - [ImageMagick – MagickCore, Low-level C API](https://imagemagick.org/script/magick-core.php)
    const magick = @cImport({
        // @cInclude("MagickCore/MagickCore.h");
        @cDefine("MAGICKCORE_HDRI_ENABLE", "1");
        @cInclude("MagickWand/MagickWand.h");
    });

    pub const PixelType = enum {
        unorm,
        half,
        float,

        fn typ(comptime self: @This()) type {
            return switch (self) {
                .unorm => u8,
                .half => f16,
                .float => f32,
            };
        }

        fn img_typ(comptime self: @This()) type {
            return Image(Pixel(self.typ()));
        }
    };
    pub fn Pixel(typ: type) type {
        return extern struct {
            r: typ,
            g: typ,
            b: typ,
            a: typ,
        };
    }
    pub fn Image(pix: type) type {
        return struct {
            buffer: []pix,
            height: usize,
            width: usize,

            pub fn deinit(self: *@This()) void {
                allocator.free(self.buffer);
            }
        };
    }

    pub const FloatImage = PixelType.float.img_typ();
    pub const HalfImage = PixelType.half.img_typ();
    pub const UnormImage = PixelType.unorm.img_typ();

    pub fn from_file(_path: []const u8, comptime typ: PixelType) !Image(Pixel(typ.typ())) {
        const path = try fspath.cwd_join(allocator.*, _path);
        defer allocator.free(path);
        var file = try std.fs.openFileAbsolute(path, .{});
        defer file.close();

        const buf = try file.readToEndAlloc(allocator.*, 50 * 1000 * 1000);
        defer allocator.free(buf);

        return decode_img(buf, typ);
    }

    pub fn decode_img(bytes: []u8, comptime typ: PixelType) !Image(Pixel(typ.typ())) {
        magick.MagickWandGenesis();
        const wand = magick.NewMagickWand() orelse {
            return error.CouldNotGetWand;
        };
        defer _ = magick.DestroyMagickWand(wand);

        // const pwand = magick.NewPixelWand() orelse {
        //     return error.CouldNotGetWand;
        // };
        // defer _ = magick.DestroyPixelWand(pwand);
        // if (magick.PixelSetColor(pwand, "#28282800") == magick.MagickFalse) {
        //     return error.CouldNotSetPWandColor;
        // }
        // if (magick.MagickSetBackgroundColor(wand, pwand) == magick.MagickFalse) {
        //     return error.CouldNotSetBgColor;
        // }

        if (magick.MagickReadImageBlob(wand, bytes.ptr, bytes.len) == magick.MagickFalse) {
            return error.CouldNotReadImage;
        }

        const img_width = magick.MagickGetImageWidth(wand);
        const img_height = magick.MagickGetImageHeight(wand);

        const buffer = try allocator.alloc(Pixel(switch (typ) {
            .unorm => u8,
            .half => f32,
            .float => f32,
        }), img_width * img_height);
        errdefer allocator.free(buffer);

        if (magick.MagickExportImagePixels(
            wand,
            0,
            0,
            img_width,
            img_height,
            "RGBA",
            switch (typ) {
                .unorm => magick.CharPixel,
                .half => magick.FloatPixel,
                .float => magick.FloatPixel,
            },
            buffer.ptr,
        ) == magick.MagickFalse) {
            return error.CouldNotRenderToBuffer;
        }

        switch (typ) {
            .half => {
                const half = try allocator.alloc(Pixel(f16), img_width * img_height);
                for (buffer, 0..) |v, i| {
                    half[i] = .{
                        .r = @floatCast(v.r),
                        .g = @floatCast(v.g),
                        .b = @floatCast(v.b),
                        .a = @floatCast(v.a),
                    };
                }
                allocator.free(buffer);
                return .{
                    .buffer = half,
                    .height = img_height,
                    .width = img_width,
                };
            },
            else => {
                return .{
                    .buffer = buffer,
                    .height = img_height,
                    .width = img_width,
                };
            },
        }
    }

    pub fn encode_rgba_image(pixels: []f32, width: usize, height: usize) ![]u8 {
        magick.MagickWandGenesis();
        const wand = magick.NewMagickWand() orelse {
            return error.CouldNotGetWand;
        };
        defer _ = magick.DestroyMagickWand(wand);

        const pwand = magick.NewPixelWand();
        defer _ = magick.DestroyPixelWand(pwand);

        _ = magick.PixelSetColor(pwand, &[_]u8{ 0, 0, 0, 0 });

        if (magick.MagickNewImage(wand, width, height, pwand) == magick.MagickFalse) {
            return error.couldnotcreatenewimage;
        }

        if (magick.MagickImportImagePixels(wand, 0, 0, width, height, "RGBA", magick.FloatPixel, pixels.ptr) == magick.MagickFalse) {
            return error.CouldNotImportImage;
        }

        if (magick.MagickSetImageFormat(wand, "PNG") == magick.MagickFalse) {
            return error.CouldNotSetFormat;
        }
        var size: usize = 0;
        const blob = magick.MagickGetImageBlob(wand, &size);
        defer _ = magick.MagickRelinquishMemory(blob);

        const cloned_blob = try allocator.dupe(u8, blob[0..size]);
        errdefer allocator.free(cloned_blob);

        return cloned_blob;
    }
};

pub const CheaderGenerator = struct {
    known_types: std.AutoHashMap(TypeId, void),
    header: std.ArrayList(u8),

    const Writer = std.ArrayList(u8).Writer;

    pub fn init() !@This() {
        var self = @This(){
            .header = std.ArrayList(u8).init(allocator.*),
            .known_types = std.AutoHashMap(TypeId, void).init(allocator.*),
        };
        errdefer self.deinit();

        try self.header.appendSlice(
            \\ // This file is generated from code. DO NOT EDIT.
            \\
            \\ #include <stdint.h>
            \\
        );

        return self;
    }

    pub fn deinit(self: *@This()) void {
        self.shader.deinit();
        self.known_types.deinit();
    }

    pub fn dump_header(self: *@This(), _path: []const u8) !void {
        const path = try fspath.cwd_join(allocator.*, _path);
        defer allocator.free(path);
        var file = try std.fs.createFileAbsolute(path, .{});
        defer file.close();

        try file.writeAll(self.shader.items);
    }

    fn zig_to_c_type(t: type) []const u8 {
        return switch (t) {
            i32 => "int32_t",
            u32 => "uint32_t",
            u16 => "uint16_t",
            f32 => "float",
            [*c]const u8 => "const char*",
            else => switch (@typeInfo(t)) {
                .array => |child| zig_to_c_type(child.child),
                else => @compileError("cannot handle this type"),
            },
        };
    }

    fn fieldname(field: std.builtin.Type.StructField) []const u8 {
        switch (@typeInfo(field.type)) {
            .array => |child| {
                const len = std.fmt.comptimePrint("{d}", .{child.len});
                return field.name ++ "[" ++ len ++ "]";
            },
            else => return field.name,
        }
    }

    fn remember(self: *@This(), t: type) !bool {
        const id = TypeId.unique(t);
        if (self.known_types.contains(id)) {
            return true;
        } else {
            try self.known_types.put(id, {});
            return false;
        }
    }

    pub fn add_struct(self: *@This(), name: []const u8, t: type) !void {
        if (try self.remember(t)) {
            return;
        }

        const w = self.shader.writer();

        switch (t) {
            i32, u32, f32 => return,
            else => switch (@typeInfo(t)) {
                .array => return,
                else => {
                    const fields = @typeInfo(t).@"struct".fields;
                    inline for (fields) |field| {
                        try self.add_struct(zig_to_c_type(field.type), field.type);
                    }

                    try w.print(
                        \\ typedef struct {{
                        \\
                    , .{});

                    inline for (fields) |field| {
                        try w.print(
                            \\     {s} {s};
                            \\
                        , .{ zig_to_c_type(field.type), fieldname(field) });
                    }

                    try w.print(
                        \\ }} {s};
                        \\
                        \\
                    , .{name});
                },
            },
        }
    }
};

pub const ShaderUtils = struct {
    const Vec4 = math.Vec4;
    const Vec3 = math.Vec3;
    const Vec2 = math.Vec2;
    const Mat4x4 = math.Mat4x4;

    pub const Mouse = extern struct { x: i32, y: i32, left: u32, right: u32 };
    pub const Camera2D = extern struct {
        eye: Vec4, // vec2 aligned
        meta: CameraMeta = .{},

        pub const CameraMeta = extern struct {
            did_move: u32 = 0,
            _pad1: u32 = 0,
            _pad2: u32 = 0,
            _pad3: u32 = 0,
        };
    };
    pub const Camera3D = extern struct {
        eye: Vec3,
        fwd: Vec3,
        right: Vec3,
        up: Vec3,
        meta: CameraMeta = .{},

        pub const CameraMeta = extern struct {
            did_change: u32 = 0,
            did_move: u32 = 0,
            did_rotate: u32 = 0,
            pad: u32 = 0,
        };
    };
    pub const Frame = extern struct {
        frame: u32,
        time: f32,
        deltatime: f32,
        width: i32,
        height: i32,
        monitor_width: i32,
        monitor_height: i32,
        pad0: u32 = 0,
    };

    // TODO: maybe enforce this
    // - [Descriptor pool and sets - Vulkan Tutorial](https://vulkan-tutorial.com/Uniform_buffers/Descriptor_pool_and_sets)
    // scalars => N (= 4 bytes given 32 bit floats).
    // vec2 => 2N (= 8 bytes)
    // vec3 or vec4 => 4N (= 16 bytes)
    // nested structure => base alignment of its members rounded up to a multiple of 16.
    // mat4 => same alignment as vec4.
    pub fn create_extern_type(comptime uniform: type) type {
        const Type = std.builtin.Type;
        const Ut: Type = @typeInfo(uniform);
        const U = Ut.@"struct";

        return @Type(.{
            .@"struct" = .{
                .layout = .@"extern",
                .fields = U.fields,
                .decls = &[_]Type.Declaration{},
                .is_tuple = false,
            },
        });
    }

    pub fn create_uniform_object(comptime uniform_type: type, uniform: anytype) create_extern_type(uniform_type) {
        const Type = std.builtin.Type;
        const Ut: Type = @typeInfo(uniform_type);
        const U = Ut.@"struct";

        var uniform_object: create_extern_type(uniform_type) = undefined;

        inline for (U.fields) |field| {
            @field(uniform_object, field.name) = @field(uniform, field.name);
        }

        return uniform_object;
    }

    pub const GlslBindingGenerator = struct {
        known_types: std.AutoHashMap(TypeId, void),
        shader: std.ArrayList(u8),

        const Writer = std.ArrayList(u8).Writer;

        pub fn init() !@This() {
            var self = @This(){
                .shader = std.ArrayList(u8).init(allocator.*),
                .known_types = std.AutoHashMap(TypeId, void).init(allocator.*),
            };
            errdefer self.deinit();

            try self.shader.appendSlice(
                \\ // This file is generated from code. DO NOT EDIT.
                \\
                \\
            );

            return self;
        }

        pub fn deinit(self: *@This()) void {
            self.shader.deinit();
            self.known_types.deinit();
        }

        fn zig_to_glsl_type(t: type) []const u8 {
            return switch (t) {
                Vec2 => "vec2",
                Vec3 => "vec3",
                Vec4 => "vec4",
                Mat4x4 => "mat4",
                i32 => "int",
                u32 => "uint",
                f32 => "float",
                Mouse => "Mouse",
                Camera2D => "Camera2D",
                Camera2D.CameraMeta => "Camera2DMeta",
                Camera3D => "Camera3D",
                Camera3D.CameraMeta => "Camera3DMeta",
                Frame => "Frame",
                else => switch (@typeInfo(t)) {
                    .array => |child| zig_to_glsl_type(child.child),
                    .@"struct" => {
                        comptime {
                            const name = @typeName(t);
                            const last = std.mem.lastIndexOfScalar(u8, name, '.') orelse @compileError("oof? " ++ name);
                            return name[last + 1 ..];
                        }
                    },
                    else => @compileError("cannot handle this type"),
                },
            };
        }

        fn fieldname(field: std.builtin.Type.StructField) []const u8 {
            switch (@typeInfo(field.type)) {
                .array => |child| {
                    const len = std.fmt.comptimePrint("{d}", .{child.len});
                    return field.name ++ "[" ++ len ++ "]";
                },
                else => return field.name,
            }
        }

        fn remember(self: *@This(), t: type) !bool {
            const id = TypeId.unique(t);
            if (self.known_types.contains(id)) {
                return true;
            } else {
                try self.known_types.put(id, {});
                return false;
            }
        }

        pub fn add_struct(self: *@This(), name: []const u8, t: type) !void {
            if (try self.remember(t)) {
                return;
            }

            const w = self.shader.writer();

            switch (t) {
                []Mat4x4, Mat4x4, Vec4, Vec3, Vec2, i32, u32, f32 => return,
                else => switch (@typeInfo(t)) {
                    .array => return,
                    else => {
                        const fields = @typeInfo(t).@"struct".fields;
                        inline for (fields) |field| {
                            try self.add_struct(comptime zig_to_glsl_type(field.type), field.type);
                        }

                        try w.print(
                            \\ struct {s} {{
                            \\
                        , .{name});

                        inline for (fields) |field| {
                            try w.print(
                                \\     {s} {s};
                                \\
                            , .{ comptime zig_to_glsl_type(field.type), fieldname(field) });
                        }

                        try w.print(
                            \\ }};
                            \\
                            \\
                        , .{});
                    },
                },
            }
        }

        pub fn add_enum(self: *@This(), prefix: []const u8, bind: type) !void {
            if (try self.remember(bind)) {
                return;
            }

            const w = self.shader.writer();

            inline for (@typeInfo(bind).@"enum".fields) |field| {
                try w.print(
                    \\ const int {s}_{s} = {d};
                    \\
                , .{ prefix, field.name, field.value });
            }

            try w.print(
                \\
                \\
            , .{});
        }

        pub fn dump_shader(self: *@This(), _path: []const u8) !void {
            const path = try fspath.cwd_join(allocator.*, _path);
            defer allocator.free(path);
            var file = try std.fs.createFileAbsolute(path, .{});
            defer file.close();

            try file.writeAll(self.shader.items);
        }
    };
};

pub const ShaderCompiler = struct {
    pub const StageMap = std.StringHashMap(Compiled);
    pub const StageSet = std.StringHashMap(bool);

    pub const Stages = struct {
        map: StageMap,

        pub fn init(compiler: *Compiler, stages: []const ShaderInfo) !@This() {
            var set = StageSet.init(allocator.*);
            defer set.deinit();
            for (stages) |stage| try set.put(stage.name, false);

            var shaders = StageMap.init(allocator.*);
            errdefer {
                var it = shaders.iterator();
                while (it.next()) |shader| {
                    shader.value_ptr.deinit();
                }
                shaders.deinit();
            }

            outer: while (true) {
                while (compiler.ctx.compiled.try_recv()) |shader| {
                    if (shaders.fetchRemove(shader.name)) |kv| {
                        kv.value.deinit();
                    }
                    try shaders.put(shader.name, shader);
                    set.getEntry(shader.name).?.value_ptr.* = true;
                }

                while (compiler.ctx.err_chan.try_recv()) |msg_| {
                    if (msg_) |msg| {
                        defer allocator.free(msg);
                        std.debug.print("{s}\n", .{msg});
                    }
                }

                var set_it = set.iterator();
                while (set_it.next()) |b| if (!b.value_ptr.*) continue :outer;
                break;
            }

            return .{ .map = shaders };
        }

        pub fn update(self: *@This(), comp: *Compiler) bool {
            const can_recv = comp.has_updates();

            while (comp.ctx.compiled.try_recv()) |shader| {
                const entry = self.map.getEntry(shader.name).?;
                entry.value_ptr.deinit();
                entry.key_ptr.* = shader.name;
                entry.value_ptr.* = shader;
            }

            while (comp.ctx.err_chan.try_recv()) |msg_| {
                if (msg_) |msg| {
                    defer allocator.free(msg);
                    std.debug.print("shader error: {s}\n", .{msg});
                } else {
                    std.debug.print("no shader errors lesgo :)\n", .{});
                }
            }

            return can_recv;
        }

        pub fn deinit(self: *@This()) void {
            var it = self.map.iterator();
            while (it.next()) |shader| {
                // shader.key_ptr; // owned by shader.value
                shader.value_ptr.deinit();
            }
            self.map.deinit();
        }
    };

    pub const ShaderInfo = struct {
        name: []const u8,
        stage: Glslc.Compiler.Stage,
        path: []const u8,
        include: []const []const u8,
        define: []const []const u8,

        fn compile(self: *const @This(), ctx: *Compiler.Ctx) ![]u32 {
            const shader: Glslc.Compiler.Code = .{ .path = .{
                .main = self.path,
                .include = self.include,
                .definitions = self.define,
            } };
            if (ctx.dump_assembly) blk: {
                // TODO: print this on screen instead of console
                const res = ctx.comp.dump_assembly(allocator.*, &shader, self.stage) catch {
                    break :blk;
                };
                switch (res) {
                    .Err => |err| {
                        try ctx.err_chan.send(err.msg);
                        return err.err;
                    },
                    .Ok => {
                        try ctx.err_chan.send(null);
                    },
                }
            }
            const frag_bytes = blk: {
                const res = try ctx.comp.compile(
                    allocator.*,
                    &shader,
                    .spirv,
                    self.stage,
                );
                switch (res) {
                    .Err => |err| {
                        try ctx.err_chan.send(err.msg);
                        return err.err;
                    },
                    .Ok => |ok| {
                        errdefer allocator.free(ok);
                        try ctx.err_chan.send(null);
                        break :blk ok;
                    },
                }
            };
            return frag_bytes;
        }
    };
    pub const Compiled = struct {
        name: []const u8,
        code: []u32,

        fn deinit(self: *const @This()) void {
            allocator.free(self.code);
            allocator.free(self.name);
        }
    };
    pub const Compiler = struct {
        const EventChan = Channel(Compiled);
        const Ctx = struct {
            comp: Glslc.Compiler,
            err_chan: Channel(?[]const u8),
            compiled: Channel(Compiled),
            shader_fuse: FsFuse,
            shaders: []ShaderInfo,
            dump_assembly: bool = false,
            has_updates: Fuse = .{},

            exit: Fuse = .{},

            fn update(self: *@This()) !void {
                var has_updates_ = false;
                defer if (has_updates_) {
                    _ = self.has_updates.fuse();
                };

                while (self.shader_fuse.try_recv()) |ev| {
                    defer ev.deinit();

                    var found_any = false;
                    for (self.shaders) |s| {
                        if (std.mem.eql(u8, s.path, ev.real)) {
                            found_any = true;
                            const res = try s.compile(self);
                            try self.compiled.send(.{
                                .name = try allocator.dupe(u8, s.name),
                                .code = res,
                            });
                        }
                    }
                    has_updates_ = has_updates_ or found_any;

                    if (!found_any) {
                        std.debug.print("Unknown file update: {s}\n", .{ev.file});
                    }
                }
            }

            fn deinit(self: *@This()) void {
                while (self.err_chan.try_recv()) |err| {
                    if (err) |er| {
                        allocator.free(er);
                    }
                }
                self.err_chan.deinit();

                while (self.compiled.try_recv()) |shader| {
                    shader.deinit();
                }
                self.compiled.deinit();
                self.shader_fuse.deinit();

                for (self.shaders) |s| {
                    allocator.free(s.name);
                    allocator.free(s.path);
                    for (s.define) |def| {
                        allocator.free(def);
                    }
                    allocator.free(s.define);
                    for (s.include) |inc| {
                        allocator.free(inc);
                    }
                    allocator.free(s.include);
                }
                allocator.free(self.shaders);
            }
        };
        ctx: *Ctx,
        thread: std.Thread,

        pub fn init(comp: Glslc.Compiler, shader_info: []const ShaderInfo) !@This() {
            const shader_fuse = try FsFuse.init("src");
            errdefer shader_fuse.deinit();

            const shaders = try allocator.dupe(ShaderInfo, shader_info);
            // OOF: not getting free-d if anything errors but meh
            for (shaders) |*s| {
                s.name = try allocator.dupe(u8, s.name);
                var buf: [std.fs.max_path_bytes:0]u8 = undefined;
                const cwd = try std.posix.getcwd(&buf);
                // s.path = try std.fs.path.join(allocator.*, &.{ cwd, s.path });
                s.path = try allocator.dupe(u8, s.path);

                const define = try allocator.alloc([]const u8, s.define.len);
                for (s.define, 0..) |def, i| {
                    define[i] = try allocator.dupe(u8, def);
                }
                s.define = define;

                const include = try allocator.alloc([]const u8, s.include.len);
                for (s.include, 0..) |inc, i| {
                    include[i] = try std.fs.path.join(allocator.*, &.{ cwd, inc });
                }
                s.include = include;
            }

            const ctxt = try allocator.create(Ctx);
            errdefer allocator.destroy(ctxt);
            ctxt.* = .{
                .comp = comp,
                .shaders = shaders,
                .shader_fuse = shader_fuse,
                .err_chan = try Channel(?[]const u8).init(allocator.*),
                .compiled = try Channel(Compiled).init(allocator.*),
            };
            errdefer ctxt.deinit();

            // no duplicates!
            {
                var set = std.StringHashMap(void).init(allocator.*);
                defer set.deinit();
                for (shaders) |s| {
                    if (set.contains(s.name)) {
                        return error.DuplicateShaderName;
                    }
                    try set.put(s.name, {});
                }
            }

            // send an event for each file
            {
                var set = std.StringHashMap(void).init(allocator.*);
                defer set.deinit();
                for (shaders) |s| {
                    if (!set.contains(s.path)) {
                        try set.put(s.path, {});
                    }
                }

                var it = set.iterator();
                while (it.next()) |s| {
                    const real = try allocator.dupe(u8, s.key_ptr.*);
                    const stripped = try std.fs.path.relative(allocator.*, ctxt.shader_fuse.ctx.path, real);
                    try ctxt.shader_fuse.ctx.channel.send(.{
                        .file = stripped,
                        .real = real,
                    });
                }
            }

            const Callbacks = struct {
                fn spawn(ctx: *Ctx) void {
                    while (true) {
                        if (ctx.exit.unfuse()) {
                            break;
                        }
                        ctx.update() catch |e| {
                            const err = std.fmt.allocPrint(allocator.*, "{any}", .{e}) catch continue;
                            ctx.err_chan.send(err) catch continue;
                        };

                        std.Thread.sleep(std.time.ns_per_ms * 100);
                    }
                }
            };
            const thread = try std.Thread.spawn(.{ .allocator = allocator.* }, Callbacks.spawn, .{ctxt});

            return .{
                .ctx = ctxt,
                .thread = thread,
            };
        }

        pub fn deinit(self: *@This()) void {
            _ = self.ctx.exit.fuse();
            self.thread.join();
            self.ctx.deinit();
            allocator.destroy(self.ctx);
        }

        pub fn has_updates(self: *@This()) bool {
            return self.ctx.has_updates.unfuse();
        }
    };
};

pub const Glslc = struct {
    pub const Compiler = struct {
        pub const Opt = enum {
            none,
            small,
            fast,
        };
        pub const Stage = enum {
            vertex,
            fragment,
            compute,
        };
        opt: Opt = .none,
        lang: enum {
            glsl,
            hlsl,
        } = .glsl,
        env: enum {
            vulkan1_3,
            vulkan1_2,
            vulkan1_1,
            vulkan1_0,
        } = .vulkan1_0,
        pub const OutputType = enum {
            assembly,
            spirv,
        };
        pub const Code = union(enum) {
            code: struct {
                src: []const u8,
                definitions: []const []const u8,
            },
            path: struct {
                main: []const u8,
                include: []const []const u8,
                definitions: []const []const u8,
            },
        };

        pub const Err = error{
            GlslcErroredOut,
        };
        pub fn CompileResult(out: OutputType) type {
            return Result(switch (out) {
                .spirv => []u32,
                .assembly => []u8,
            }, Err);
        }

        pub fn dump_assembly(
            self: @This(),
            alloc: std.mem.Allocator,
            code: *const Code,
            stage: Stage,
        ) !Result(void, Err) {
            // std.debug.print("{s}\n", .{code});
            const res = try self.compile(alloc, code, .assembly, stage);
            switch (res) {
                .Ok => |bytes| {
                    defer alloc.free(bytes);
                    std.debug.print("{s}\n", .{bytes});
                    return .Ok;
                },
                .Err => |err| {
                    return .{ .Err = err };
                },
            }
        }

        pub fn compile(
            self: @This(),
            alloc: std.mem.Allocator,
            code: *const Code,
            comptime output_type: OutputType,
            stage: Stage,
        ) !CompileResult(output_type) {
            var args = std.ArrayList([]const u8).init(alloc);
            defer {
                for (args.items) |arg| {
                    alloc.free(arg);
                }
                args.deinit();
            }
            if (builtin.os.tag == .windows) {
                try args.append(try fspath.cwd_join(alloc, "zig-out/bin/glslc.exe"));
            } else {
                try args.append(try alloc.dupe(u8, "glslc"));
            }
            try args.append(try alloc.dupe(u8, switch (stage) {
                .fragment => "-fshader-stage=fragment",
                .vertex => "-fshader-stage=vertex",
                .compute => "-fshader-stage=compute",
            }));
            try args.append(try alloc.dupe(u8, switch (self.lang) {
                .glsl => "-xglsl",
                .hlsl => "-xhlsl",
            }));
            try args.append(try alloc.dupe(u8, switch (self.env) {
                .vulkan1_3 => "--target-env=vulkan1.3",
                .vulkan1_2 => "--target-env=vulkan1.2",
                .vulkan1_1 => "--target-env=vulkan1.1",
                .vulkan1_0 => "--target-env=vulkan1.0",
            }));
            try args.append(try alloc.dupe(u8, switch (self.opt) {
                .fast => "-O",
                .small => "-Os",
                .none => "-O0",
            }));
            if (output_type == .assembly) {
                try args.append(try alloc.dupe(u8, "-S"));
            }
            try args.append(try alloc.dupe(u8, "-o-"));
            switch (code.*) {
                .code => |src| {
                    for (src.definitions) |def| {
                        try args.append(try std.fmt.allocPrint(alloc, "-D{s}", .{def}));
                    }
                    try args.append(try alloc.dupe(u8, "-"));
                },
                .path => |paths| {
                    for (paths.definitions) |def| {
                        try args.append(try std.fmt.allocPrint(alloc, "-D{s}", .{def}));
                    }
                    for (paths.include) |inc| {
                        try args.append(try alloc.dupe(u8, "-I"));
                        try args.append(try fspath.replace_sep(inc));
                    }
                    try args.append(try fspath.replace_sep(paths.main));
                },
            }

            // for (args.items) |arg| {
            //     std.debug.print("{s} ", .{arg});
            // }
            // std.debug.print("\n", .{});

            var child = std.process.Child.init(args.items, alloc);
            child.stdin_behavior = .Pipe;
            child.stdout_behavior = .Pipe;
            child.stderr_behavior = .Pipe;

            try child.spawn();

            const stdin = child.stdin orelse return error.NoStdin;
            child.stdin = null;
            const stdout = child.stdout orelse return error.NoStdout;
            child.stdout = null;
            const stderr = child.stderr orelse return error.NoStderr;
            child.stderr = null;
            defer stdout.close();
            defer stderr.close();

            switch (code.*) {
                .code => |src| {
                    try stdin.writeAll(src.src);
                },
                .path => {},
            }
            stdin.close();

            // similar to child.collectOutput
            const max_output_bytes = 1000 * 1000;
            var poller = std.io.poll(allocator.*, enum { stdout, stderr }, .{
                .stdout = stdout,
                .stderr = stderr,
            });
            defer poller.deinit();

            while (try poller.poll()) {
                if (poller.fifo(.stdout).count > max_output_bytes)
                    return error.StdoutStreamTooLong;
                if (poller.fifo(.stderr).count > max_output_bytes)
                    return error.StderrStreamTooLong;
            }

            const err = try child.wait();
            blk: {
                var err_buf = std.ArrayList(u8).init(alloc);

                switch (err) {
                    .Exited => |e| {
                        if (e != 0) {
                            _ = try err_buf.writer().print("exited with code: {}\n", .{e});
                        } else {
                            err_buf.deinit();
                            break :blk;
                        }
                    },
                    // .Signal => |code| {},
                    // .Stopped => |code| {},
                    // .Unknown => |code| {},
                    else => |e| {
                        try err_buf.writer().print("exited with code: {}\n", .{e});
                    },
                }

                const fifo = poller.fifo(.stderr);
                try err_buf.appendSlice(fifo.buf[fifo.head..][0..fifo.count]);
                return .{
                    .Err = .{
                        .err = Err.GlslcErroredOut,
                        .msg = try err_buf.toOwnedSlice(),
                    },
                };
            }

            const fifo = poller.fifo(.stdout);
            var aligned = std.ArrayListAligned(u8, 4).init(allocator.*);
            try aligned.appendSlice(fifo.buf[fifo.head..][0..fifo.count]);
            const bytes = try aligned.toOwnedSlice();
            return .{ .Ok = switch (output_type) {
                .spirv => std.mem.bytesAsSlice(u32, bytes),
                .assembly => bytes,
            } };
        }
    };
};

pub const Remotery = struct {
    const c = @cImport({
        @cDefine("RMT_ENABLED", "1");
        @cDefine("RMT_USE_VULKAN", "1");
        @cInclude("Remotery.h");
    });

    const VulkanContext = @import("engine.zig").VulkanContext;
    const vk = @import("vulkan");

    rmt: *c.Remotery,
    samples: std.StringHashMap(c.rmtU32),

    bind: *c.rmtVulkanBind,

    pub fn init(ctx: *VulkanContext) !@This() {
        var rmt: *c.Remotery = undefined;
        const code = c._rmt_CreateGlobalInstance(@ptrCast(&rmt));
        if (code != c.RMT_ERROR_NONE) {
            std.debug.print("code: {d}\n", .{code});
            return error.ErrorInitializingRMT;
        }
        errdefer c._rmt_DestroyGlobalInstance(rmt);

        // TODO: vulkan is broken. not sure if it's remotery or me.
        //    - remotery's vulkan support is not yet in a release
        const funcs: c.rmtVulkanFunctions = .{
            .vkGetPhysicalDeviceProperties = @ptrCast(@constCast(ctx.instance.wrapper.dispatch.vkGetPhysicalDeviceProperties)),
            .vkQueueSubmit = @ptrCast(@constCast(ctx.device.wrapper.dispatch.vkQueueSubmit)),
            .vkQueueWaitIdle = @ptrCast(@constCast(ctx.device.wrapper.dispatch.vkQueueWaitIdle)),
            .vkCreateQueryPool = @ptrCast(@constCast(ctx.device.wrapper.dispatch.vkCreateQueryPool)),
            .vkDestroyQueryPool = @ptrCast(@constCast(ctx.device.wrapper.dispatch.vkDestroyQueryPool)),
            .vkResetQueryPool = @ptrCast(@constCast(ctx.device.wrapper.dispatch.vkResetQueryPool)),
            .vkGetQueryPoolResults = @ptrCast(@constCast(ctx.device.wrapper.dispatch.vkGetQueryPoolResults)),
            .vkCmdWriteTimestamp = @ptrCast(@constCast(ctx.device.wrapper.dispatch.vkCmdWriteTimestamp)),
            .vkCreateSemaphore = @ptrCast(@constCast(ctx.device.wrapper.dispatch.vkCreateSemaphore)),
            .vkDestroySemaphore = @ptrCast(@constCast(ctx.device.wrapper.dispatch.vkDestroySemaphore)),
            .vkSignalSemaphore = @ptrCast(@constCast(ctx.device.wrapper.dispatch.vkSignalSemaphore)),
            .vkGetSemaphoreCounterValue = @ptrCast(@constCast(ctx.device.wrapper.dispatch.vkGetSemaphoreCounterValue)),
            .vkGetCalibratedTimestampsEXT = @ptrCast(@constCast(ctx.device.wrapper.dispatch.vkGetCalibratedTimestampsKHR)),
        };

        var bind: ?*c.rmtVulkanBind = null;
        if (c._rmt_BindVulkan(
            @ptrFromInt(@intFromEnum(ctx.instance.handle)),
            @ptrFromInt(@intFromEnum(ctx.pdev)),
            @ptrFromInt(@intFromEnum(ctx.device.handle)),
            @ptrFromInt(@intFromEnum(ctx.graphics_queue.handle)),
            &funcs,
            @ptrCast(&bind),
        ) != c.RMT_ERROR_NONE) return error.ErrorBindingVulkan;

        return .{ .rmt = rmt, .samples = .init(allocator.*), .bind = bind orelse return error.ErrorBindingVulkan };
    }

    pub fn deinit(self: *@This()) void {
        c._rmt_UnbindVulkan(self.bind);
        c._rmt_DestroyGlobalInstance(self.rmt);
        self.samples.deinit();
    }

    pub fn mark_frame(_: *@This()) !void {
        if (c._rmt_MarkFrame() != c.RMT_ERROR_NONE) return error.ErrorMarkingFrame;
    }

    pub fn begin_vulkan_sample(self: *@This(), cmdbuf: vk.CommandBuffer, name: [:0]const u8) void {
        const sample = self.samples.getOrPut(name) catch @panic("OOM");
        if (!sample.found_existing) {
            sample.value_ptr.* = 0;
        }
        c._rmt_BeginVulkanSample(self.bind, @ptrFromInt(@intFromEnum(cmdbuf)), name.ptr, sample.value_ptr);
    }

    pub fn end_vulkan_sample(_: *@This()) void {
        c._rmt_EndVulkanSample();
    }

    pub fn begin_sample(self: *@This(), name: [:0]const u8) void {
        const sample = self.samples.getOrPut(name) catch @panic("OOM");
        if (!sample.found_existing) {
            sample.value_ptr.* = 0;
        }
        c._rmt_BeginCPUSample(name.ptr, 0, sample.value_ptr);
    }

    pub fn end_sample(_: *@This()) void {
        c._rmt_EndCPUSample();
    }

    pub fn log(_: *@This(), msg: [:0]const u8) void {
        c._rmt_LogText(msg.ptr);
    }
};

pub const Tracy = struct {
    const options = @import("tracy-options");
    const c = @cImport({
        if (options.tracy_enable) @cDefine("TRACY_ENABLE", {});
        if (options.on_demand) @cDefine("TRACY_ON_DEMAND", {});
        if (options.callstack) @cDefine("TRACY_CALLSTACK", {});
        if (options.no_callstack) @cDefine("TRACY_NO_CALLSTACK", {});
        if (options.no_callstack_inlines) @cDefine("TRACY_NO_CALLSTACK_INLINES", {});
        if (options.only_localhost) @cDefine("TRACY_ONLY_LOCALHOST", {});
        if (options.no_broadcast) @cDefine("TRACY_NO_BROADCAST", {});
        if (options.only_ipv4) @cDefine("TRACY_ONLY_IPV4", {});
        if (options.no_code_transfer) @cDefine("TRACY_NO_CODE_TRANSFER", {});
        if (options.no_context_switch) @cDefine("TRACY_NO_CONTEXT_SWITCH", {});
        if (options.no_exit) @cDefine("TRACY_NO_EXIT", {});
        if (options.no_sampling) @cDefine("TRACY_NO_SAMPLING", {});
        if (options.no_verify) @cDefine("TRACY_NO_VERIFY", {});
        if (options.no_vsync_capture) @cDefine("TRACY_NO_VSYNC_CAPTURE", {});
        if (options.no_frame_image) @cDefine("TRACY_NO_FRAME_IMAGE", {});
        if (options.no_system_tracing) @cDefine("TRACY_NO_SYSTEM_TRACING", {});
        if (options.patchable_nopsleds) @cDefine("TRACY_PATCHABLE_NOPSLEDS", {});
        if (options.delayed_init) @cDefine("TRACY_DELAYED_INIT", {});
        if (options.manual_lifetime) @cDefine("TRACY_MANUAL_LIFETIME", {});
        if (options.fibers) @cDefine("TRACY_FIBERS", {});
        if (options.timer_fallback) @cDefine("TRACY_TIMER_FALLBACK", {});
        if (options.no_crash_handler) @cDefine("TRACY_NO_CRASH_HANDLER", {});
        if (options.libunwind_backtrace) @cDefine("TRACY_LIBUNWIND_BACKTRACE", {});
        if (options.symbol_offline_resolve) @cDefine("TRACY_SYMBOL_OFFLINE_RESOLVE", {});
        if (options.libbacktrace_elf_dynload_support) @cDefine("TRACY_LIBBACKTRACE_ELF_DYNLOAD_SUPPORT", {});
        if (options.verbose) @cDefine("TRACY_VERBOSE", {});
        if (options.debuginfod) @cDefine("TRACY_DEBUGINFOD", {});
        if (options.shared) @cDefine("DTRACY_EXPORTS", {});
        if (builtin.os.tag == .windows and options.shared) @cDefine("WINVER", "0x0601");
        if (builtin.os.tag == .windows and options.shared) @cDefine("_WIN32_WINNT", "0x0601");
        @cInclude("tracy/TracyC.h");
    });

    samples: std.ArrayList(ZoneContext),
    depth: u32 = 5,

    pub fn init() !@This() {
        return .{ .samples = .init(allocator.*) };
    }

    pub fn deinit(self: *@This()) void {
        self.samples.deinit();
    }

    pub inline fn begin_sample(self: *@This(), comptime src: std.builtin.SourceLocation, comptime name: [:0]const u8) void {
        const opts = comptime ZoneOptions{ .name = name };
        const active: c_int = @intFromBool(opts.active);

        const S = struct {
            // NOTE: this is marked static in the original C macros
            // and this is how you have static local varialbles in zig.
            var src_loc = c.___tracy_source_location_data{
                .name = if (opts.name) |n| n.ptr else null,
                .function = src.fn_name.ptr,
                .file = src.file,
                .line = src.line,
                .color = opts.color orelse 0,
            };
        };

        if (!options.no_callstack) {
            if (options.callstack) {
                return .{
                    .ctx = c.___tracy_emit_zone_begin_callstack(&S.src_loc, self.depth, active),
                };
            }
        }

        self.samples.append(.{
            .ctx = c.___tracy_emit_zone_begin(&S.src_loc, active),
        }) catch @panic("OOM");
    }

    pub inline fn end_sample(self: *@This()) void {
        const sample = self.samples.pop() orelse return;
        sample.end();
    }

    pub fn mark_frame(_: *@This()) !void {
        c.___tracy_emit_frame_mark(null);
    }

    pub fn log(self: *@This(), msg: [:0]const u8) void {
        c.___tracy_emit_messageL(msg, self.depth);
    }

    pub inline fn plot_config(name: [:0]const u8, config: PlotConfig) void {
        c.___tracy_emit_plot_config(
            name,
            @intFromEnum(config.plot_type),
            @intFromBool(config.step),
            @intFromBool(config.fill),
            config.color,
        );
    }

    pub inline fn plot(self: *@This(), name: [:0]const u8, value: anytype) void {
        const type_info = @typeInfo(@TypeOf(value));
        switch (type_info) {
            .comptime_int => self.plot(name, @as(i64, value)),
            .comptime_float => self.plot(name, @as(f64, value)),
            .int => |int_type| {
                if (int_type.bits > 64) @compileError("Too large int to plot");
                c.___tracy_emit_plot_int(name, @intCast(value));
            },
            .float => |float_type| {
                if (float_type.bits <= 32) {
                    c.___tracy_emit_plot_float(name, @floatCast(value));
                } else if (float_type.bits <= 64) {
                    c.___tracy_emit_plot(name, @floatCast(value));
                } else {
                    @compileError("Too large float to plot");
                }
            },
            else => @compileError("Unsupported plot value type"),
        }
    }

    pub const PlotType = enum(c.TracyPlotFormatEnum) {
        Number = c.TracyPlotFormatNumber,
        Memory = c.TracyPlotFormatMemory,
        Percentage = c.TracyPlotFormatPercentage,
        Watt = c.TracyPlotFormatWatt,
    };

    pub const PlotConfig = struct {
        plot_type: PlotType,
        step: bool,
        fill: bool,
        color: u32,
    };

    pub const ZoneOptions = struct {
        active: bool = true,
        name: ?[]const u8 = null,
        color: ?u32 = null,
    };

    pub const ZoneContext = struct {
        ctx: c.___tracy_c_zone_context,

        pub inline fn end(zone: ZoneContext) void {
            c.___tracy_emit_zone_end(zone.ctx);
        }

        pub inline fn name(zone: ZoneContext, zone_name: []const u8) void {
            c.___tracy_emit_zone_name(zone.ctx, zone_name.ptr, zone_name.len);
        }

        pub inline fn text(zone: ZoneContext, zone_text: []const u8) void {
            c.___tracy_emit_zone_text(zone.ctx, zone_text.ptr, zone_text.len);
        }

        pub inline fn color(zone: ZoneContext, zone_color: u32) void {
            c.___tracy_emit_zone_color(zone.ctx, zone_color);
        }

        pub inline fn value(zone: ZoneContext, zone_value: u64) void {
            c.___tracy_emit_zone_value(zone.ctx, zone_value);
        }
    };
};

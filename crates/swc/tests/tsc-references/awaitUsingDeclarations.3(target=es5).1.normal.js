//// [awaitUsingDeclarations.3.ts]
import { _ as _async_to_generator } from "@swc/helpers/_/_async_to_generator";
import { _ as _define_property } from "@swc/helpers/_/_define_property";
import { _ as _ts_generator } from "@swc/helpers/_/_ts_generator";
import { _ as _ts_add_disposable_resource } from "@swc/helpers/_/_ts_add_disposable_resource";
import { _ as _ts_dispose_resources } from "@swc/helpers/_/_ts_dispose_resources";
{
    var env = {
        stack: [],
        error: void 0,
        hasError: false
    };
    try {
        var d1 = _ts_add_disposable_resource(env, _define_property({}, Symbol.asyncDispose, function() {
            return _async_to_generator(function() {
                return _ts_generator(this, function(_state) {
                    return [
                        2
                    ];
                });
            })();
        }), true), d2 = _ts_add_disposable_resource(env, null, true), d3 = _ts_add_disposable_resource(env, undefined, true), d4 = _ts_add_disposable_resource(env, _define_property({}, Symbol.dispose, function() {}), true);
    } catch (e) {
        env.error = e;
        env.hasError = true;
    } finally{
        var result = _ts_dispose_resources(env);
        if (result) await result;
    }
}export { };

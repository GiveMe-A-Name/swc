//// [awaitBinaryExpression1_es5.ts]
import { _ as _async_to_generator } from "@swc/helpers/_/_async_to_generator";
import { _ as _ts_generator } from "@swc/helpers/_/_ts_generator";
function func() {
    return _async_to_generator(function() {
        var b;
        return _ts_generator(this, function(_state) {
            switch(_state.label){
                case 0:
                    before();
                    return [
                        4,
                        p
                    ];
                case 1:
                    b = _state.sent() || a;
                    after();
                    return [
                        2
                    ];
            }
        });
    })();
}

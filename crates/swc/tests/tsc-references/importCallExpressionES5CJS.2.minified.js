//// [0.ts]
function foo() {
    return "foo";
}
Object.defineProperty(exports, "__esModule", {
    value: !0
}), Object.defineProperty(exports, "foo", {
    enumerable: !0,
    get: function() {
        return foo;
    }
});
//// [1.ts]
Object.defineProperty(exports, "__esModule", {
    value: !0
});
var target = exports, all = {
    get D () {
        return D;
    },
    get p2 () {
        return p2;
    }
};
for(var name in all)Object.defineProperty(target, name, {
    enumerable: !0,
    get: Object.getOwnPropertyDescriptor(all, name).get
});
var _class_call_check = require("@swc/helpers/_/_class_call_check"), _interop_require_wildcard = require("@swc/helpers/_/_interop_require_wildcard");
Promise.resolve().then(function() {
    return /*#__PURE__*/ _interop_require_wildcard._(require("./0"));
}), Promise.resolve().then(function() {
    return /*#__PURE__*/ _interop_require_wildcard._(require("./0"));
}).then(function(zero) {
    return zero.foo();
});
var p2 = Promise.resolve().then(function() {
    return /*#__PURE__*/ _interop_require_wildcard._(require("./0"));
}), D = /*#__PURE__*/ function() {
    function D() {
        _class_call_check._(this, D);
    }
    return D.prototype.method = function() {
        Promise.resolve().then(function() {
            return /*#__PURE__*/ _interop_require_wildcard._(require("./0"));
        });
    }, D;
}();

//// [initializerReferencingConstructorLocals.ts]
// Initializer expressions for instance member variables are evaluated in the scope of the class constructor body but are not permitted to reference parameters or local variables of the constructor. 
import { _ as _class_call_check } from "@swc/helpers/_/_class_call_check";
var C = function C(x) {
    "use strict";
    _class_call_check(this, C);
    this.a = z; // error
    this.c = this.z; // error
    z = 1;
};
var D = function D(x) {
    "use strict";
    _class_call_check(this, D);
    this.a = z; // error
    this.c = this.z; // error
    z = 1;
};

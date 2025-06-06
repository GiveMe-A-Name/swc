//// [requires.d.ts]
//// [mod1.js]
/// <reference path='./requires.d.ts' />
module.exports.bothBefore = 'string';
module.exports = {
    justExport: 1,
    bothBefore: 2,
    bothAfter: 3
};
module.exports.bothAfter = 'string';
module.exports.justProperty = 'string';
//// [a.js]
/// <reference path='./requires.d.ts' />
var mod1 = require('./mod1');
mod1.justExport.toFixed();
mod1.bothBefore.toFixed(); // error, 'toFixed' not on 'string | number'
mod1.bothAfter.toFixed(); // error, 'toFixed' not on 'string | number'
mod1.justProperty.length;

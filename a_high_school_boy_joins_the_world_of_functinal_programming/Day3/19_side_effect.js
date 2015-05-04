var a = true;

var f = function(x)
{
    a = false;
    return x + 1;
};

console.log(a); // true
var x = f(1);
console.log(a); // false

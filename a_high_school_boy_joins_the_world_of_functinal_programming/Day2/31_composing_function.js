var increment = function(x)
{
    return x + 1;
};

var after4 = increment(5);

console.log(after4);

var double = function(x)
{
    return x * 2;
};

var double_increment = function(x)
{
    return increment(double(x));
};

var after5 = double_increment(5);

console.log(after5);

var increment_double = function(x)
{
    return double(increment(x));
};

var after6 = increment_double(5);

console.log(after6);

var f = increment;
var g = double;

var compose = function(f, g)
{
    var fg = function(x)
    {
        return g(f(x));
    };

    return fg;
};

var fg = compose(f, g);
var gf = compose(g, f);
var ff = compose(f, f);
var gg = compose(g, g);

console.log(f(5));
console.log(g(5));
console.log(fg(5));
console.log(gf(5));
console.log(ff(5));
console.log(gg(5));

var fggf = compose(fg, gf);
console.log(fggf(5));

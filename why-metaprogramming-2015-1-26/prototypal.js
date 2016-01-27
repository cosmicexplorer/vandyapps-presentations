function filterOutZeros(array) {
  return array.filter(function (element) {
    return element !== 0;
  });
}

Array.prototype.filterOutZeros = function () {
  return filterOutZeros(this);
};

console.log([0, 1, 0, 2, 0, 3].filterOutZeros());

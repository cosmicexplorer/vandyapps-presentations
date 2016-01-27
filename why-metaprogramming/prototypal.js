function filterOutZeros(array) {
  return array.filter(function (element) {
    return element !== 0;
  });
}

Array.prototype.filterOutZeros = function () {
  return filterOutZeros(this);
};

String.prototype.replaceAll = function(search, replacement) {
    var target = this;
    return target.split(search).join(replacement);
};

for (var i=0; i<size(); i++) {
    var item = str(read(i));
    print("(" + i + ")" + " " + item.replaceAll("\\n", "(newline)") + "\\n");
}

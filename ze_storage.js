function Storage() {
	this._data = {}
}

Storage.prototype.get = function(url) {
	return this._data[url];
};

Storage.prototype.getByPrefix = function(prefix) {
	var l = prefix.length;
	var ret = {};
	for(k in this._data) {
		if(k.substring(0, l) != prefix) {
			continue;
		}
		if(!Object.prototype.hasOwnProperty.call(this._data, k)) {
			continue;
		}
		ret[k] = this._data[k];
	}
	return ret;
};

Storage.prototype.set = function(url, v) {
	this._data[url] = v;
	return this;
};

Storage.prototype.setProperty = function(url, prop, v) {
	this._data[url][prop] = v;
	return this;
};

Storage.prototype.del = function(url) {
	if(url in this._data) {
		delete this._data[url];
	}
	return this;
};

exports.storage = new Storage();


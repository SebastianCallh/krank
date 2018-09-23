'use strict;'

exports.playAudio = function (audio) {
    return function () {
	audio.play()
    }
}

exports.loadAudio = function (filePath) {
    return function () {
	new Audio(filePath);
    }
}

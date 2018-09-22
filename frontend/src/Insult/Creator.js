'use strict;'

exports.playSound = function () {
    return function () {
	new Audio('air-horn.mp3').play();
    }
}

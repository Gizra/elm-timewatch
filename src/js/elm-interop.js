"use strict";

function isTouchDevice() {
  return 'ontouchstart' in window || 'onmsgesturechange' in window;
}

var initialValues = {
  isTouchDevice: isTouchDevice()
};

var elmApp = Elm.fullscreen(Elm.Main, initialValues);

elmApp.ports.isTouchDevice.send(initialValues.isTouchDevice);

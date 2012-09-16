window.animationFrame = (function() {
return window.requestAnimationFrame || 
       window.webkitRequestAnimationFrame || 
       window.mozRequestAnimationFrame || 
       window.msRequestAnimationFrame;
})();

function init() {
  var canvas = resizer();
  animate({canvas: canvas,
           context: canvas.getContext("2d"),
           x: canvas.width/2,
           y: canvas.height/2,
           d: 2,
           dx: 1,
           dy: 1});
}

function resizer() {
  var canvas = document.getElementById("aCanvas");

  var pw = canvas.parentNode.clientWidth;
  var ph = canvas.parentNode.clientHeight;

  canvas.height = ph * 0.8;
  canvas.width = pw * 0.8;
  canvas.style.top = (ph-canvas.height)/2 + "px";
  canvas.style.left = (pw-canvas.width)/2 + "px";

  return canvas;
}

function animate(s) {
  // update stage
  if ( !(0 <= s.x && s.x <= s.canvas.width) ) { s.dx *= -1 }
  if ( !(0 <= s.y && s.y <= s.canvas.height) ) { s.dy *= -1 }

  s.x += s.dx;
  s.y += s.dy;

  // clear stage
  s.context.clearRect(0, 0, s.canvas.width, s.canvas.height);

  // render stage
  s.context.fillRect(s.x-s.d, s.y-s.d, s.d, s.d);

  // request new frame
  animationFrame(function() { animate(s); });
}

window.onload = function() {
  init();
};

window.onresize = function() {
  resizer();
};

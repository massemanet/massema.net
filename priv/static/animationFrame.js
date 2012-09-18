window.animationFrame =
    (function() {
         return window.requestAnimationFrame ||
             window.webkitRequestAnimationFrame ||
             window.mozRequestAnimationFrame ||
             window.msRequestAnimationFrame;})();

function drawCircle(ctx,x,y,r,color) {
    var startAngle = 0;
    var endAngle   = 2*Math.PI;

    ctx.beginPath();
    ctx.arc(x,y,r,startAngle,endAngle);
    ctx.fillStyle = color;
    ctx.fill();
}

function init() {
  var canvas = resizer();
  animate({canvas: canvas,
           context: canvas.getContext("2d"),
           x: canvas.width/2,
           y: canvas.height/2,
           d: 8,
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
  if ( s.x < 0 )               { s.dx =  1; s.x = 0; }
  if ( s.canvas.width < s.x )  { s.dx = -1; s.x = s.canvas.width; }
  if ( s.y < 0 )               { s.dy =  1; s.y = 0; }
  if ( s.canvas.height < s.y ) { s.dy = -1; s.y = s.canvas.height; }

  s.x += s.dx;
  s.y += s.dy;

  // clear stage
  s.context.clearRect(0, 0, s.canvas.width, s.canvas.height);

  // render stage
  drawCircle(s.context,s.x,s.y,s.d,"white");

  // request new frame
  animationFrame(function() { animate(s); });
}

window.onload = function() {
  init();
};

window.onresize = function() {
  resizer();
};

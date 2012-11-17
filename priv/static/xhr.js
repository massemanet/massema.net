function load() {
  function tickhandler(oEvent) {
    document.getElementById('tick').innerHTML = oEvent.srcElement.response;
    ajax();
  };

  function ajax() {
    var oReq = new XMLHttpRequest();
    oReq.open("GET", "tick", true);
    oReq.onloadend = tickhandler;
    oReq.send(null);
  };

  ajax();
}
window.onload = load;

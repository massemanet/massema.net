function load() {
  function mkhandler() {
    return function(oEvent) {
      console.log(this);
      console.log(oEvent);
    };
  }
  var url = "http://massema.net/erl/mm/do";
  var oReq = new XMLHttpRequest();
  oReq.open("GET", url, true);
  oReq.onloadend = mkhandler(oReq);
  //  oReq.onreadystatechange = mkhandler();
  oReq.send(null);
}
window.onload = load;

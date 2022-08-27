import { Elm } from "../src/Worker/Main.elm";


// hack to get around Platform.worker + parcel bug
// https://github.com/parcel-bundler/parcel/issues/4905
self.document = {
  body: "foo", 
  getBodyById: function(){}
};

let app = Elm.Worker.Main.init({
  flags: {}
});

app.ports.elmToJs.subscribe(function(msg) {
  console.log("sub msg", msg);
  postMessage(msg);
});

onmessage = function(msg) {
  console.log("worker.js got msg", msg);

  app.ports.jsToElm.send(msg.data);
}

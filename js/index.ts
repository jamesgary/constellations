import { Elm } from "../src/Main.elm";

let workerUrl = new URL('worker.js', import.meta.url);
let worker = new Worker(workerUrl);
let lsKey = "constellations";

let app = Elm.Main.init({
  node: document.getElementById("elm-node"),
  flags: {
    timestamp: Date.now(),
    localStorage: JSON.parse(localStorage.getItem(lsKey)),
  },
});

app.ports.elmToJs.subscribe(function(msg) {
  switch (msg.id) {
    case 'Save':
      console.log("Saving...");
      save(msg.localStorage);
      break;
    case 'WorkerMsg':
      console.log("index.ts got msg from Main.elm", msg);
      worker.postMessage(msg.msg);
      break;
  }
});

function save(ls) {
  localStorage.setItem(lsKey, JSON.stringify(ls));
}

worker.onmessage = function(event) {
  console.log("index.ts got msg from worker", event.data);
  app.ports.jsToElm.send(event.data);
};


/*
worker.onmessage = function(event) {
  var edgeData = event.data;
  console.log("Sending edge data:", edgeData);
  app.ports.loadedLevelFresh.send(edgeData);
};

function loadLevel(difficulty: int) {
  var savedLevelProgress = localStorage.getItem("c:levelProgress:" + difficulty);
  if (savedLevelProgress) {
    try {
      levelProgress = JSON.parse(savedLevelProgress)
      app.ports.loadedLevelInProgress.send([levelProgress, difficulty]);
    } catch (e) {
      console.error("Couldn't parse current level progress", e);
      worker.postMessage(difficulty);
    }
  } else {
    if (difficulty === 1) {
      // sometimes we can randomly generate a presolved level,
      // so just hardcode the first one
      console.log("Sending hardcoded edge data for difficulty 1:", firstLevel);
      app.ports.loadedLevelFresh.send(firstLevel);
    } else {
      console.log("Generating difficulty of " + difficulty + "...");
      worker.postMessage(difficulty);
    }
  }
}

function checkForIntersections(nodes, edges) {
  console.log("Checking for intersections...");

  var intersectionResults = getIntersectionResults(nodes, edges);
  app.ports.intersectionResults.send(intersectionResults);
}

app.ports.saveConfig.subscribe(function(config) {
  console.log("Saving config:", config);
  localStorage.setItem("c:config:radius", config.radius);
});

app.ports.checkForIntersections.subscribe(function(nodesAndEdgeDataAndDiff) {
});

function getIntersectionResults(nodes, edgeData) {
  // clear all the edges
  var hasIntersections = false;
  for (var i = 0; i < edges.length; i++) {
    edges[i].overlappingEdges = [];
  }

  for (var i = 0; i < edges.length; i++) {
    var edge1 = edges[i];
    for (var j = i + 1; j < edges.length; j++) {
      var edge2 = edges[j];

      // check if edges share a node (skip if so)
      var e1n1 = nodes[edge1.pair[0]];
      var e1n2 = nodes[edge1.pair[1]];
      var e2n1 = nodes[edge2.pair[0]];
      var e2n2 = nodes[edge2.pair[1]];

      if (e1n1.id !== e2n1.id &&
          e1n1.id !== e2n2.id &&
          e1n2.id !== e2n1.id &&
          e1n2.id !== e2n2.id) {

        if (isIntersecting(e1n1.dest, e1n2.dest, e2n1.dest, e2n2.dest)) {
          edges[i].overlappingEdges.push(edge2.id);
          edges[j].overlappingEdges.push(edge1.id);
          hasIntersections = true;
        }
      }
    }
  }

  return [hasIntersections, edges];
}

function isIntersecting(p1, p2, p3, p4) {
  // See https://gist.github.com/Joncom/e8e8d18ebe7fe55c3894
  var x1 = p1.x;
  var y1 = p1.y;
  var x2 = p2.x;
  var y2 = p2.y;
  var x3 = p3.x;
  var y3 = p3.y;
  var x4 = p4.x;
  var y4 = p4.y;

  var s1_x = x2 - x1;
  var s1_y = y2 - y1;
  var s2_x = x4 - x3;
  var s2_y = y4 - y3;

  var thing = (-s2_x * s1_y + s1_x * s2_y);

  var s = (-s1_y * (x1 - x3) + s1_x * (y1 - y3)) / thing;
  var t = ( s2_x * (y1 - y3) - s2_y * (x1 - x3)) / thing;

  return (s >= 0 && s <= 1 && t >= 0 && t <= 1);
}

var firstLevel = [[
    {"id":0,"pair":[3,4],"overlappingEdges":[]},
    {"id":1,"pair":[4,2],"overlappingEdges":[]},
    {"id":2,"pair":[3,1],"overlappingEdges":[]},
    {"id":3,"pair":[1,5],"overlappingEdges":[]},
    {"id":4,"pair":[4,1],"overlappingEdges":[]},
    {"id":5,"pair":[1,0],"overlappingEdges":[]},
    {"id":6,"pair":[2,5],"overlappingEdges":[]},
    {"id":7,"pair":[5,0],"overlappingEdges":[]},
  ], 6, 1,
];
*/

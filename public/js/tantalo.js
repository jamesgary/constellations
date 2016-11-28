Tantalo = function() {
  return {
    // difficulty starts at 0
    generateTantalo: function(difficulty) {
      var numLines = 5 + difficulty;

      var lines = this.generateRandomLines(numLines); // just simple slope-intercept form, (y = mx + b)

      // make a bunch of lines around a circle
      // get all intersections (id, x, y, [L1, L2])
      var intersections = this.findIntersections(lines);

      var edgeData = this.getEdgeData(lines);

      this.randomizeEdgeData(edgeData, intersections.length);
      return [edgeData, intersections.length, difficulty];
    },

    // private

    generateRandomLines: function(numLines) {
      var lines = [];
      for (var i = 0; i < numLines; i++) {
        lines.push(this.generateRandomLine());
      }

      return lines;
    },

    generateRandomLine: function() {
      var maxSlope = 10;
      var maxYIntercept = 100;

      var slope = maxSlope * this.rand();
      var yIntercept = maxYIntercept * this.rand();

      // I think these make the slope range evenly distributed
      if (this.rand() > 0.5) { slope = 1 / slope; }
      if (this.rand() > 0.5) { slope *= -1; }

      return {
        slope: slope,
        yIntercept: yIntercept,
        intersections: [], // will use in future
      };
    },

    findIntersections: function(lines) {
      var intersections = [];
      var id = 0;
      for (var i = 0; i < lines.length - 1; i++) {
        var line1 = lines[i];
        var slope1 = line1.slope;
        var yIntercept1 = line1.yIntercept;

        for (var j = i + 1; j < lines.length - 1; j++ && j != i) {
          var line2 = lines[j];
          var slope2 = line2.slope;
          var yIntercept2 = line2.yIntercept;

          // mx + b = mx + b
          // solve for x by isolating x on left side
          var newB = yIntercept2 - yIntercept1; // mx = mx + B
          var newM = slope1 - slope2; // Mx = B
          var x = newB / newM;
          var y = (line1.slope * x) + line1.yIntercept; // apply original line formula

          var intersection = {
            id: id,
            x: x,
            y: y,
            line1: line1,
            line2: line2,
          };
          intersections.push(intersection);
          line1.intersections.push(intersection);
          line2.intersections.push(intersection);

          id++;
        }
      }
      return intersections;
    },

    getEdgeData: function(lines) {
      var edges = [];

      for (var i = 0; i < lines.length - 1; i++) {
        var line = lines[i];

        // sort by x
        line.intersections.sort(function(a, b) {
          a.x - b.x;
        });

        for (var k = 0; k < line.intersections.length - 1; k++) {
          edges.push([
            line.intersections[k].id,
            line.intersections[k + 1].id
          ]);
        }
      }

      return edges;
    },

    rand: function() {
      return Math.random();
    },

    randomizeEdgeData: function(edgeData, numNodes) {
      var randomIds = [];
      for (var i = 0; i < numNodes; i++) {
        randomIds.push(i);
      }
      this.shuffle(randomIds);
      var idToRandomId = function(origId) {
        return randomIds[origId];
      };

      for (var i = 0; i < edgeData.length; i++) {
        var node1 = edgeData[i][0];
        var node2 = edgeData[i][1];
        edgeData[i] = [
          idToRandomId(node1),
          idToRandomId(node2),
        ]
      }
      return edgeData;
    },

    // http://stackoverflow.com/a/6274381
    shuffle: function(a) {
      var j, x, i;
      for (i = a.length; i; i--) {
        j = Math.floor(Math.random() * i);
        x = a[i - 1];
        a[i - 1] = a[j];
        a[j] = x;
      }
    }
  };
};

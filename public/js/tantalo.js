Tantalo = function() {
  return {
    generateTantalo: function(difficulty) {
      let numLines = difficulty;

      let lines = this.generateRandomLines(numLines); // just simple slope-intercept form, (y = mx + b)

      // make a bunch of lines around a circle
      // get all intersections (id, x, y, [L1, L2])
      let intersections = this.findIntersections(lines);

      let edgeData = this.getEdgeData(lines, intersections);
      return [edgeData, intersections.length];
    },

    // private

    generateRandomLines: function(numLines) {
      let lines = [];
      for (let i = 0; i < numLines; i++) {
        lines.push(this.generateRandomLine());
      }

      return lines;
    },

    generateRandomLine: function() {
      let maxSlope = 10;
      let maxYIntercept = 100;

      let slope = maxSlope * this.rand();
      let yIntercept = maxYIntercept * this.rand();

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
      let intersections = [];
      let id = 0;
      for (let i = 0; i < lines.length - 1; i++) {
        let line1 = lines[i];
        let slope1 = line1.slope;
        let yIntercept1 = line1.yIntercept;

        for (let j = i + 1; j < lines.length - 1; j++ && j != i) {
          let line2 = lines[j];
          let slope2 = line2.slope;
          let yIntercept2 = line2.yIntercept;

          // mx + b = mx + b
          // solve for x by isolating x on left side
          let newB = yIntercept2 - yIntercept1; // mx = mx + B
          let newM = slope1 - slope2; // Mx = B
          let x = newB / newM;
          let y = (line1.slope * x) + line1.yIntercept; // apply original line formula

          let intersection = {
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

    getEdgeData: function(lines, intersections) {
      let edges = [];

      for (let i = 0; i < lines.length - 1; i++) {
        let line = lines[i];

        // sort by x
        line.intersections.sort(function(a, b) {
          a.x - b.x;
        });

        for (let k = 0; k < line.intersections.length - 1; k++) {
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
    }
  };
};

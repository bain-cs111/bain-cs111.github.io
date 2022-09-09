/* 
	Source: http://nardove.com/
	Original code created by @nardove. We asked him for permission to use
	his Jellies, but please do not reproduce without permission.
*/
paper.install(window);
paper.setup('my_canvas');

const random = (min, max, intOnly) => {
	const num = Math.random() * (max - min) + min;
	if (intOnly) {
		return Math.floor(num);
	}
	return num;
};

const jellyColors = [
	{ stroke: "#1C4347", fill: "#49ACBB" },
	{ stroke: "#1b3b3a", fill: "#61cac8" },
	{ stroke: "#2d393f", fill: "#88a5b3" },
	{ stroke: "#422b3a", fill: "#b0809e" },
	{ stroke: "#5b263a", fill: "#d85c8a" },
	{ stroke: "#580c23", fill: "#ff3775" },
	{ stroke: "#681635", fill: "#EB1962" }
];
const screenW = view.size.width;
const screenH = view.size.height;
const jellies = [];

const getRandomColor = () => {
  return jellyColors[random(0, jellyColors.length, true)];
};

const addJelly = () => {
  if (jellies.length >= 4) {
    console.log('enough jellies...');
    return;
  }
  console.log('adding a jelly...')
  
  const jelly = new Jelly({
        radius: random(45, 75), 
        resolution: random(14, 18), 
        color: getRandomColor(), 
        tentacleLength: random(4, 6), 
        x: random(-200, 0), 
        y: random(-20, screenH + 50)
    });
  jellies.push(jelly);
};

//animate jellies for each frame:
view.onFrame = (event) => {
    counter=0;
	  while(counter < jellies.length) {
	      jellies[counter].move(event);
	      ++counter;
    }
};

// add a jelly every 10 seconds:
addJelly();
setInterval(addJelly, 10000);
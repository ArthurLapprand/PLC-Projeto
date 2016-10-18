function len(a, numero){
	var z = [];
	if(a.equals(z)){
		return numero;
	} else {
		var num1 = numero + 1;
		var k = a.tail;
		return len(k,num1);
	}
}
/*function div (array ,starting,length){
	var temp = [];
	for(var x = starting; x<length; x++){
		temp = temp.concat(array[x]);
	}
	return temp;
}
function merge (left, right){
	var x = len(left, 0);
	var y = len(righ, 0);
	var res = [];
	var countX = 0, countY = 0;
	while(x > 0 && y > 0){
		if(left[countX] <= right[countY]){
			res = res.concat(left[x]);
			x--;
		}else{
			res = res.concat(right[y]);
			y--;
		}
	}
	while(x >0){
		res = res.concat(left[x]);
		x--;
	}
	while(y>0){
		res = res.concat(right[y]);
		y--;
	}
	return res;
}

function mergeSort (array) {
	if(len(array,0) == 2){
		return array;
	}
	arrayLen = len(array,0);
	var meio = arrayLen / 2;
	var left = div(array,0,meio);
	var right = div(array,meio,arrayLen);
	return merge(mergeSort(left),mergeSort(right));

}*/
function ola (x){
	if(x[0] < -50){
		return x[0];
	}
	var b = 50;
	var y = [x[0] -b];
	return ola(y);
}
a = [1,2,3];
var y = len(a,0);
//// = 0;
//w = [1];
//y = len(x,w);
//ola([1]);
/*var x = 0;
while (true) {
	if (x > 5) break;
	else x++;
}*/

/*var i;
var j = 50;
for (i = 0; i < 3; i++) {
	j = j - i;
}
j;*/

/*var x = [1,2,3], y = 10;
x[1] + y;
// lol
y++;
return;
var z = function(a, b) {
	return (a + b);
}

z(1,2);

for (var i = 0; i < 3; i++) {
	x[i];
}
x[i-1];

x = [];

y = [];

x.equals(y);

x = [1,2,3];
x[1] = 3;
x.head;*/
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
function div (array,starting,length){
	var temp = [];
	for(var x = starting; x<length; x++){
		temp = temp.concat(array[x]);
	}
	return temp;
}
function merge (left, right){
	var x = len(left, 0);
	var y = len(right, 0);
	var res = [];
	var countX = 0, countY = 0;
	while(x > 0 && y > 0){
		if(left[countX] <= right[countY]){
			res = res.concat(left[countX]);
			countX++;
			x--;
		}else{
			res = res.concat(right[countY]);
			y--;
			countY++;
		}
	}
	while(x >0){
		res = res.concat(left[countX]);
		countX++;
		x--;
	}
	while(y>0){
		res = res.concat(right[countY]);
		countY++;
		y--;
	}
	return res;
}

function mergeSort (array) {
	if(len(array,0) == 1){
		return array;
	}
	arrayLen = len(array,0);
	var meio = arrayLen / 2;
	var left = div(array,0,meio);
	var right = div(array,meio,arrayLen);
	return merge(mergeSort(left),mergeSort(right));
}

x = mergeSort([1,5,6,20,-1,0,-1,10]);
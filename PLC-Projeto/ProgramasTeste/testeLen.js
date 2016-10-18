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

x = [1,3,5,7];
y = len(x,0);
y;
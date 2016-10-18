function fatorial(x) {
	if (x == 0) return 1;
	return x * fatorial(x - 1);
}
y = fatorial(3);
y;
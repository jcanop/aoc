#ifndef ARRAYS_H_
#define ARRAYS_H_

#define array_sort(type, array) {\
	int len = sizeof(array) / sizeof(array[0]);\
	int i, j; type temp;\
	for (i = 0; i < len; i++)\
		for (j = i + 1; j < len; j++)\
			if (array[i] > array[j]) {\
				temp = array[i];\
				array[i] = array[j];\
				array[j] = temp;\
			}\
}

#define array_reverse(type, array) {\
	int len = sizeof(array) / sizeof(array[0]);\
	int i; type temp;\
	for (i = 0; i < len / 2; i++) {\
		temp = array[i];\
		array[i] = array[len - i - 1];\
		array[len - i - 1] = temp;\
	}\
}

#define array_print(array) {\
	int len = sizeof(array) / sizeof(array[0]);\
	int i; for (i = 0; i < len; i++) printf("%d\n", array[i]);\
}

#endif /* ARRAYS_H_ */

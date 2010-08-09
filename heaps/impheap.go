/***
 * 
 * impheap.go
 * Imperative heap implemented using arrays
 * Based on the priority queue implementation in 
 * "The Algorithm Design Manual", 2nd edition, by Steve Skiena
 * 
 * Andrei de A. Formiga, 2010-08-09
 *
 ********************************************************************/

package main

import "fmt"

const PQSIZE = 1000

// assume a heap of ints
type Heap struct { 
	n	int		// current size of heap
	queue	[PQSIZE]int	// heap array
}

func newHeap() *Heap {
	h := new(Heap)
	h.n = 0
	return h
}

func parent(n int) int {
	if n <= 1 {
		return -1
	}
 
	return n / 2		// implicitly floor(n/2)
}

func young_child(n int) int {
	return n*2
}

func swap(h *Heap, ix1, ix2 int) {
	tmp := h.queue[ix1]
	h.queue[ix1] = h.queue[ix2]
	h.queue[ix2] = tmp
}

func bubble_up(h *Heap, ix int) {
	p := parent(ix)
	if p > 0 && h.queue[p] > h.queue[ix] {
		swap(h, p, ix)
		bubble_up(h, p)
	}
}

func insert(h *Heap, x int) {
	h.n = h.n + 1
	h.queue[h.n] = x
	bubble_up(h, h.n)
}

func make_heap(s []int) *Heap {
	h := newHeap()
	for _, v := range s {
		insert(h, v)
	}

	return h
}

func min_ix(h *Heap, ix1, ix2 int) int {
	if h.queue[ix1] < h.queue[ix2] {
		return ix1
	}

	return ix2
}

func local_min_ix(h *Heap, ix int) int {
	ch := young_child(ix)
	if ch + 1 <= h.n {
		return min_ix(h, ix, min_ix(h, ch, ch+1))
	} else if ch <= h.n {
		return min_ix(h, ix, ch)
	}

	return ix
}

func bubble_down(h *Heap, ix int) {
	min_ix := local_min_ix(h, ix)
	if min_ix != ix {
		swap(h, ix, min_ix)
		bubble_down(h, min_ix)
	}
}

func extract_min(h *Heap) int {
	if h.n <= 0 {
		fmt.Printf("extract_min called on Empty heap\n")
		return -1
	}
	
	min := h.queue[1]
	h.queue[1] = h.queue[h.n]
	h.n = h.n - 1
	bubble_down(h, 1)

	return min
}

func heap_sort(s []int) {
	h := make_heap(s)
	for i, _ := range s {
		s[i] = extract_min(h)
	}
}

func print_heap(h *Heap) {
	fmt.Printf("[")
	for i := 1; i < h.n; i++ {
		fmt.Printf("%d ", h.queue[i])
	}
	fmt.Printf("%d]\n", h.queue[i])
}

func main() {
	s := []int{77, 33, 45, 12, 89, 135, 66, 111, 902}
	//h := make_heap(s)

	fmt.Printf("Initial array: ")
	for _, v := range s {
		fmt.Printf("%d ", v)
	}
	fmt.Printf("\n")

	heap_sort(s)

	fmt.Printf("Sorted array: ")
	for _, v := range s {
		fmt.Printf("%d ", v)
	}
	fmt.Printf("\n")
}

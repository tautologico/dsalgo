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

func make_heap(s []int, n int) *Heap {
	h := newHeap()
	for _, v := range s {
		insert(h, v)
	}

	return h
}

func min_ix(h *heap, ix1, ix2 int) int {
	if h.queue[ix1] < h.queue[ix2] {
		return ix1
	}

	return ix2
}

func main() {
	s := []int{77, 33, 45, 12, 89, 135, 66, 111, 902}

	fmt.Printf("shong")
}

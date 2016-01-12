/*
 
   impheap.go
   Imperative heap implemented using arrays
   Based on the priority queue implementation in 
   "The Algorithm Design Manual", 2nd edition, by Steve Skiena
  
   Andrei de A. Formiga, 2010-08-09
 
*/

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

func youngChild(n int) int {
	return n*2
}

func swap(h *Heap, ix1, ix2 int) {
	tmp := h.queue[ix1]
	h.queue[ix1] = h.queue[ix2]
	h.queue[ix2] = tmp
}

func bubbleUp(h *Heap, ix int) {
	p := parent(ix)
	if p > 0 && h.queue[p] > h.queue[ix] {
		swap(h, p, ix)
		bubbleUp(h, p)
	}
}

func insert(h *Heap, x int) {
	h.n = h.n + 1
	h.queue[h.n] = x
	bubbleUp(h, h.n)
}

func makeHeap(s []int) *Heap {
	h := newHeap()
	for _, v := range s {
		insert(h, v)
	}

	return h
}

func minIx(h *Heap, ix1, ix2 int) int {
	if h.queue[ix1] < h.queue[ix2] {
		return ix1
	}

	return ix2
}

// finds which node has smallest value between the node at index ix
// and its two children in the heap
func localMinIx(h *Heap, ix int) int {
	ch := youngChild(ix)
	if ch + 1 <= h.n {
		return minIx(h, ix, minIx(h, ch, ch+1))
	} else if ch <= h.n {
		return minIx(h, ix, ch)
	}

	return ix
}

func bubbleDown(h *Heap, ix int) {
	minIx := localMinIx(h, ix)
	if minIx != ix {
		swap(h, ix, minIx)
		bubbleDown(h, minIx)
	}
}

func extractMin(h *Heap) int {
	if h.n <= 0 {
		fmt.Printf("extract_min called on Empty heap\n")
		return -1
	}
	
	min := h.queue[1]
	h.queue[1] = h.queue[h.n]
	h.n = h.n - 1
	bubbleDown(h, 1)

	return min
}

func heapSort(s []int) {
	h := makeHeap(s)
	for i, _ := range s {
		s[i] = extractMin(h)
	}
}

func printHeap(h *Heap) {
	fmt.Printf("[")
	for i := 1; i < h.n; i++ {
		fmt.Printf("%d ", h.queue[i])
	}
	fmt.Printf("%d]\n", h.queue[len(h.queue) - 1])
}

func main() {
	s := []int{77, 33, 45, 12, 89, 135, 66, 111, 902}
	//h := make_heap(s)

	fmt.Printf("Initial array: ")
	for _, v := range s {
		fmt.Printf("%d ", v)
	}
	fmt.Printf("\n")

	heapSort(s)

	fmt.Printf("Sorted array: ")
	for _, v := range s {
		fmt.Printf("%d ", v)
	}
	fmt.Printf("\n")
}

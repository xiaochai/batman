package main

import (
	"container/heap"
	"container/list"
	"fmt"
	"math"
	"math/rand"
	"time"
)

type Data struct {
	Condition int
	Value     int
	Weight    float64 // 原生的权重
	Key       float64 // 关键值
}

func main() {
	rand.Seed(time.Now().UnixNano())
	l := list.New()
	for i := 0; i < 100; i++ {
		var weight = 0.2
		if i < 50 {
			weight = 1 // 设置不同的权重
		}
		_ = weight
		l.PushBack(&Data{
			i, //rand.Intn(1000),
			i,
			float64(weight),
			0.0,
		})
	}
	res := map[int]int{}
	for i := 0; i < 50000; i++ {
		//t := WSRARes(l, 3, -1);
		t := WSRExpJ(l, 3, -1)
		for _, v := range t {
			res[v.Value]++

		}
	}
	sum := 0
	for i := 0; i < 100; i++ {
		fmt.Println(i, res[i])
		sum += res[i]

	}
	fmt.Println("sum", sum)

}

func WSRExpJ(l *list.List, m int, cond int) []*Data {
	h := PHeap{}
	heap.Init(&h)

	var XW float64
	var flightSum float64
	for e := l.Front(); e != nil; e = e.Next() {
		data, ok := e.Value.(*Data)
		if !ok || data.Condition <= cond {
			continue
		}
		data.Key = math.Pow(rand.Float64(), 1/data.Weight)
		if h.Len() < m {
			heap.Push(&h, data)
			if h.Len() == m { // 当插入完所有元素之后，算出xw值
				XW = math.Log(rand.Float64()) / math.Log(h[0].Key)
			}
		} else if flightSum += data.Weight; flightSum >= XW {
			tw := math.Pow(h[0].Key, data.Weight)
			r2 := rand.Float64()*(1-tw) + tw
			data.Key = math.Pow(r2, 1/data.Weight)
			heap.Pop(&h)
			heap.Push(&h, data)
			flightSum = 0
			XW = math.Log(rand.Float64()) / math.Log(h[0].Key)
		}
	}
	return h
}

// 加权随机抽样
func WSRARes(l *list.List, m int, cond int) []*Data {
	h := PHeap{}
	heap.Init(&h)

	for e := l.Front(); e != nil; e = e.Next() {
		data, ok := e.Value.(*Data)
		if !ok || data.Condition <= cond {
			continue
		}
		data.Key = math.Pow(rand.Float64(), 1/data.Weight)
		if h.Len() < m {
			heap.Push(&h, data)
		} else if h[0].Key < data.Key {
			heap.Pop(&h)
			heap.Push(&h, data)
		}
	}
	return h
}

// 最小堆的实现
type PHeap []*Data

func (h PHeap) Len() int {
	return len(h)
}

func (h PHeap) Less(i, j int) bool {
	return h[i].Key < h[j].Key
}

func (h PHeap) Swap(i, j int) {
	h[i], h[j] = h[j], h[i]
}

func (h *PHeap) Push(x interface{}) {
	d := x.(*Data)
	*h = append(*h, d)
}
func (h *PHeap) Pop() interface{} {
	item := (*h)[h.Len()-1]
	*h = (*h)[0 : h.Len()-1]
	return item
}

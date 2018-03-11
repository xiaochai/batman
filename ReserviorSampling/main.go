package main

import (
	"container/list"
	"fmt"
	"math/rand"
	"time"
)

type Data struct {
	Condition int
	Value     int
}

func main() {
	rand.Seed(time.Now().UnixNano())
	l := list.New()
	for i := 0; i < 100; i++ {
		l.PushBack(&Data{
			rand.Intn(1000),
			i,
		})
	}
	res := map[int]int{}
	for i := 0; i < 500000; i++ {
		t := ReservoirSample(l, 3, 0)
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

// 随机获取l中满足条件的m个元素，
func ReservoirSample(l *list.List, m int, cond int) []*Data {
	var des []*Data
	i := 1
	for e := l.Front(); e != nil; e = e.Next() {
		data, ok := e.Value.(*Data)
		if !ok || data.Condition <= cond {
			continue
		}
		if i <= m {
			des = append(des, data)
		} else {
			if pos := rand.Intn(i); pos < m {
				des[pos] = data
			}
		}
		i++
	}
	return des
}

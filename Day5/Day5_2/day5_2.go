package main

import (
	"os"
	"bufio"
	"strconv"
	"fmt"
)

func readLines(path string) ([]string, error) {
	file, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer file.Close()
	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines, scanner.Err()
}

func main() {
	lines, _ := readLines("../input.txt")
	max := len(lines)
	maze := make([]int, 0, max)
	for _, line := range lines {
		num, _ := strconv.Atoi(line)
		maze = append(maze, num)
	}
	min := 0
	fmt.Println(jumpAround(0, maze, max, min, 0))
}

func jumpAround(position int, maze[]int, max int, min int, steps int)(int){
	pos := position
	for pos < max && pos >= min {
		offset := maze[pos]
		if offset >= 3 {
			maze[pos] -= 1
		} else {
			maze[pos] += 1
		}
		pos += offset
		steps++
	}
	return steps
}


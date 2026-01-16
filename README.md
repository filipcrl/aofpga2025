# Advent of FPGA 2025

This is my submission to the [2025 Jane Street Advent of FPGA](https://blog.janestreet.com/advent-of-fpga-challenge-2025/) challenge. I decided to solve the first part of day 7 using Hardcaml.

## Running the project

```bash
opam install hardcaml ppx_hardcaml
dune test
```

## Introduction to the Problem

On the seventh day of Advent of Code, you are tasked with repairing the broken teleporter. First, a beam is generated from the 'S' symbol. Then, it propagates downwards. If it reaches a '^' symbol called a splitter, it is stopped from propagating downwards and split into two beams on the right and left sides of the splitter. You must count the number of times the tachyon beam is split in order to solve the puzzle.

```
.......S.......
.......|.......
......|^|......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
```

## First attempt

The most obvious way to approach this problem in hardware is to stream the lines and keep the current state of the beams in a register. Then, the next row of beams is a combinational function of the previous beam state and of the current line of splitters. This is pictured below for the second and third rows of the example shown in the previous section.

<picture>
  <img src="/assets/combinational.png" alt="Test image" style="width: 80%; display: block; margin: 0 auto;">
</picture>

The issue with doing it this way is that the number of resources needed to implement the design on the FPGA grows in proportion to the width of the line. So, if you want to process very long lines, you need to be prepared to spend a lot of money.

## Current Design

<picture>
  <img src="/assets/dataflow.png" alt="Data flow" style="width: 80%; display: block; margin: 0 auto;">
</picture>

To process bigger lines without increasing the area used, my idea was to split each line into equally sized words. The specific length of the words is configurable through a functor. The resulting beam state would also be split into words and stored inside dual-port BRAM memory that is available on many FPGAs. Inside the BRAM, double buffering is used. Half of the address space is reserved for reads and half for writes. The two halves are swapped every line, removing the need for copying the data.

A major complication comes from the fact that, at the edges of the words, we need beam information from the neighboring words in order to compute the resulting beam state. There are a couple of approaches to handle this, but as a design constraint, I wanted the design to always be ready to accept data and to handle not receiving data for an arbitrary number of clock cycles, regardless of the internal state.

In addition, reading from the memory is synchronous, adding a 1-clock-cycle delay. Because I need to read two words in order to compute the result, there is a minimum latency of 2 clock cycles. In order to keep the throughput high, both the input data and the data read from the memory are pipelined. This is where the main complexity of the design comes from. The general shape of the pipelines is shown below.

<picture>
  <img src="/assets/pipelines.png" alt="Pipeline" style="width: 80%; display: block; margin: 0 auto;">
</picture>

This design also has the added feature that, when data is available to complete the output of a line, it enters the `Flush` state and ensures the line is output as soon as possible, regardless of whether the data for the next line is ready.

## Performance

I achieved a sustained throughput of 1 word per cycle, translating to an effective column throughput of `word_w * f_clk`. The maximum latency is 2 cycles when input data is available every clock cycle.

## Testing

I used Hardcaml's Cyclesim with expect-tests in order to ensure my design works. I started with small, hand-checkable inputs to arrive at a functional design. Then, I moved on to bigger inputs in order to fully test the correctness of my block. Then, I was inspired by the expect tests to create a visualizer for the propagation of beams. This proved to be very useful when debugging the trickier parts of my design, allowing me to see the exact place where something went wrong, without staring for hours at waveforms.

<picture>
  <img src="/assets/badtree.png" alt="Example of failing test" style="width: 80%; display: block; margin: 0 auto;">
</picture>

I also created and tested the Advent of Code input, but with random 0-10 cycle gaps between the valid input words. This let me discover more bugs and made me more confident that the design works as intended.

## Possible Improvements

Right now, the throughput is limited by the maximum word size of BRAM on the FPGA. Expanding on the concept I developed, I could use multiple memories in parallel (banking), increasing the word size and allowing me to scale my design to process many more columns in parallel, while also allowing very long lines, limited only by memory capacity.

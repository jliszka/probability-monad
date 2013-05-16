#!/bin/sh

scalac -d out Distribution.scala Examples.scala && scala -cp out -Yrepl-sync -i boot.scala

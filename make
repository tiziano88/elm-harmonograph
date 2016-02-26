#!/bin/bash

protoc --elm_out=. proto/*.proto
elm make ./harmonograph.elm

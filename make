#!/bin/bash

protoc --elm_out=. ./Proto/*.proto
elm make ./harmonograph.elm

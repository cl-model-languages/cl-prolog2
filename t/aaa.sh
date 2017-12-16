#!/bin/bash

swipl -l aaa.pl --quiet

yap -l aaa.pl
# somehow yap -q disables all messages


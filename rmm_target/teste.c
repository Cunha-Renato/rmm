#include<stdio.h>
int main(int argv, char** argc){ int num_ = 0; scanf("%d", &num_); for (int i_ = 0; i_ < num_; i_+=1) { printf("%d", (i_ * num_));} if (num_ == 0) {  printf("%d", 1); } return 0;}
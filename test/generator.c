// https://github.com/Esteban795

#include <stdlib.h>
#include <stdio.h>

int rand_between(int l, int r) {
  return (int)( (rand() / (RAND_MAX * 1.0f)) * (r - l) + l);
}


void write_file(FILE* f,int n,int l,int r){
    int temp_1,temp_2;
    for (int i = 0; i < n;i++){
        temp_1 = rand_between(l,r);
        temp_2 = rand_between(l,r);
        fprintf(f,"%d %d\n",temp_1,temp_2);
    }
}

int main(int argc,char* argv[]){
    if (argc != 5) return EXIT_FAILURE;
    FILE* out_f = fopen(argv[1],"w");
    int n = atoi(argv[2]);
    int low_bound = atoi(argv[3]);
    int high_bound = atoi(argv[4]);
    write_file(out_f,n,low_bound,high_bound);
    return 0;
}

//gcc generator.c -o fg -Wall -Wvla -Wextra -fsanitize=address

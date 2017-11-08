#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#define RED   "\x1B[31m"
#define GRN   "\x1B[32m"
#define YEL   "\x1B[33m"
#define BLU   "\x1B[34m"
#define MAG   "\x1B[35m"
#define CYN   "\x1B[36m"
#define WHT   "\x1B[39m"
#define RESET "\x1B[0m"
#define max 500

float bestfitness;
float prevDistance;
int bestOrder[max];
int bestFitIndex;
float bestDistance;
int k;
int size;
int verification_count,verification_max;
float fit[max],normal;
float mean;
int generation;
double besttime=0;
double elapsedtime;
time_t start_t, end_t;

void Genpoints();
void shuffle(int a[max]);
void GenPop();
void FitPopulation();
void SelectPopulation();
void mutate();
void evolve();

float calculatedistance(int order[max]);
float distance(int x0,int x1,int y0,int y1);
float dist(int i);

float Max(float a[max]);
float performanceDensity();

void swap(int i,int a,int b);
void swap_array(int array[max],int a,int b);
void copy(int a[max],int b[max]);

void viewpoints();
void showdists();


struct point
{
        int x,y;
} points[max];


struct pop
{
        int order[max];
} *population;


float Max(float a[max])
{
        int i;
        float best=0;
        for(i=0; i<size; i++)
                if(a[i]>best)
                        best=a[i];
        return(best);
}


void shuffle(int a[max])
{
        int i,temp,r1,r2;
        for(i=1; i<10; i++)
        {
                r1=random()%k;
                r2=random()%k;

                if(r1>0&&r2>0&&r1<k-1&&r2<k-1)
                {
                        temp=a[r1];
                        a[r1]=a[r2];
                        a[r2]=temp;
                }
        }
        // for(i=0; i<k; i++)
        // printf("%d ",a[i] );
}


void GenPop()
{
        int i,j;
        for(i=0; i<size; i++)
                for(j=0; j<=k-2; j++)
                {
                        population[i].order[j]=j;
                        population[i].order[k-1]=0;
                }

}


void Genpoints()
{
        int i;
        points[0].x=0;
        points[0].y=0;
        points[k-1].x=0;
        points[k-1].x=0;
        for (i=1; i<k-1; i++)
                points[i].x=random()%10;
        for(i=1; i<k-1; i++)
                points[i].y=random()%10;
}


void swap(int i,int a,int b)
{
        int temp;
        temp=population[i].order[a];
        population[i].order[a]=population[i].order[b];
        population[i].order[b]=temp;

}

void swap_array(int array[max],int a,int b)
{
        int temp;
        temp=array[a];
        array[a]=array[b];
        array[b]=temp;
}

float distance(int x0,int x1,int y0,int y1)
{
        float dist;
        int x2,y2;
        x2=(x0-x1)*(x0-x1);
        y2=(y0-y1)*(y0-y1);

        dist=sqrt(x2+y2);

        return(dist);
}




float dist(int i)
{
        int m;
        float sum,d;
        sum=0;
        for(m=0; m<k-1; m++)
        {
                d=distance(points[population[i].order[m]].x,points[population[i].order[m+1]].x,points[population[i].order[m]].y,points[population[i].order[m+1]].y);
                sum=sum+d;

        }
        return sum;
}


void FitPopulation()
{
        int i,j;
        float d,sum;

        for(i=0; i<size; i++)
        {
                sum=dist(i);
                fit[i]=1/log10(sum);

        }

}


void mutate()
{
        int i,j,r1,r2,*order,d;
        order=(int *)malloc(k*sizeof(int));
        for(i=1; i<size; i++)
        {
                copy(order,population[i].order);
                d=dist(i);
                for(j=0; j<10*abs((int)(mean-fit[i])); j++)
                {
                        r1=random()%k+1;
                        r2=random()%k+1;
                        if(r1<k-1&&r2<k-1)
                                swap(i,r1,r2);
                        if(d<dist(i))
                                copy(population[i].order,order);
                        r1=random()%k+1;
                        r2=random()%k+1;
                }
        }
        free(order);
}



float performanceDensity()
{
        int i,c=0;
        float d=0;
        for(i=0; i<size; i++)
                d=d+dist(i);
        d=d/size;
        mean=1/log10(d);
        for(i=0; i<size; i++)
                if(dist(i)>d)
                        c++;
        return(c);
}
void viewpoints()
{
        int i;
        printf(WHT "Points: ");
        for(i=0; i<k; i++)
        {
                printf(WHT "%d,",points[i].x);
                printf("%d|",points[i].y);

        }
        printf("\n\n");

}


void copy(int a[max],int b[max])
{
        int i;
        for(i=0; i<k; i++)
                a[i]=b[i];
}


void SelectPopulation()
{
        int i,j;
        FitPopulation();
        bestfitness=Max(fit);
        for(i=0; i<size; i++)
        {

                if(fit[i]==bestfitness)
                {
                        printf(CYN "");
                        bestFitIndex=i;
                }

        }
        if(dist(bestFitIndex)<bestDistance)
        {
                copy(population[0].order,population[bestFitIndex].order);
                bestFitIndex=0;
        }
        if(bestDistance>dist(0))
        {
                time(&end_t);
                besttime = difftime(end_t, start_t);
                verification_count=0;

        }
        else verification_count+=1;
        bestDistance=dist(0);
}


float calculatedistance(int order[max])
{
        int i;
        float d=0;
        for(i=0; i<k-1; i++)
        {
                d=d+distance(points[order[i]].x,points[order[i+1]].x,points[order[i]].y,points[order[i+1]].y);
        }
        return d;
}


void evolve()
{
        int i,var,p,order[max];
        for(i=1; i<size; i++)
        {
                copy(order,population[i].order);
                for(var=random()%k+1; var<k-1; var++)
                {
                        for(p=rand()%k+1; p<k-1; p++)
                        {
                                swap_array(order,var,p);
                                if(calculatedistance(order)<dist(i))
                                        copy(population[i].order,order);
                                else
                                        copy(order,population[i].order);
                        }
                }
        }
        p=1;
        for(i=1; i<size; i++)
        {
                var=rand()%size+1;
                copy(order,population[i].order);
                if(p<k-2)
                {
                        swap(var,p,p+1);
                        if(dist(i)<calculatedistance(order)) ;
                        else copy(population[i].order,order);
                }
                else p=1;
                p++;
                var=rand()%size+1;
        }
        SelectPopulation();
        generation++;
}


void showdists()
{
        int i,j;
        float d;
        for(i=0; i<25; i++)
        {
                d=dist(i);

                printf(GRN "");
                for(j=0; j<k; j++)
                        printf("%d ",population[i].order[j]);
                printf(WHT "d:%f\n",d );

        }
        printf("\n");
}
void main()
{
        int i,j,m;
        float pd;
        printf("Number of Points:");
        scanf("%d",&m);
        time(&start_t);
        k=m+2;
        Genpoints();
        size=300;
        population=(struct pop *)malloc(size*sizeof(struct pop));
        verification_max=1000;
        GenPop();
        for(i=0; i<size; i++)
                shuffle(population[i].order);


        SelectPopulation();
        printf(YEL "\nBest fitness: %f\n",bestfitness);
        verification_count=0;
        m=1;
        printf(CYN "Best Performance: %f\n",dist(bestFitIndex));
        while(verification_count<verification_max)
        {
                system("clear");
                printf("\n" );
                viewpoints();
                printf("Total Points: %d\n",k);
                pd=performanceDensity();
                printf(BLU "Generation:%d\tGFitness:%f\tMean:%f\n",generation,pd/size,mean);

                printf(YEL "\nBest fitness: %f\n",bestfitness);

                printf(CYN "Best Performance: %f\t",bestDistance);
                printf(CYN "Time:%ds\n",(int)besttime);

                copy(bestOrder,population[bestFitIndex].order);
                printf(MAG "Best Sequence: ");
                for(i=0; i<k; i++)
                        printf("%d ",bestOrder[i] );
                printf("\n");
                printf(WHT "UC Check: %d%c\t",(100*verification_count/verification_max),'%' );
                time(&end_t);
                elapsedtime=difftime(end_t,start_t);
                printf(WHT "Time Elapsed:%dm %ds\n\n",(int)elapsedtime/60,(int)elapsedtime%60);

                showdists();
                evolve();
                if(generation/100>m)
                {
                        m++;
                        mutate();
                }
        }
        SelectPopulation();
        printf(YEL "\nBest fitness: %f\n",bestfitness);
        printf(CYN "Best Performance: %f\n",dist(bestFitIndex));
        printf(MAG "Best Sequence: ");
        for(i=0; i<k; i++)
                printf("%d ",bestOrder[i] );
        printf("\n");
        time(&end_t);
        elapsedtime=difftime(end_t,start_t);
        printf(CYN "Best Time:%ds\n",(int)besttime);
        printf(WHT "Time Elapsed:%dm %ds\n",(int)elapsedtime/60,(int)elapsedtime%60);
}

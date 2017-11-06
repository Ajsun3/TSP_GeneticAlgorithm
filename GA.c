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
#define WHT   "\x1B[37m"
#define RESET "\x1B[0m"
#define max 5000
#define max_size 20000

float mean=0;
float bestfitness;
float prevDistance;
float d;
int bestOrder[max];
int bestFitIndex;
float bestDistance;
int k;
int size;
int verification_count,verification_max;
float fit[max],normal;
int order1[max],order2[max],order3[max],m1,m2,m3;
int mutationrate=10;
int evolutionrate;
int rateconst=2;
int generation;
double besttime=0;
double elapsedtime;
time_t start_t, end_t;


struct point
{
        int x,y;
} points[max];


struct pop
{
        int order[max];
} population[max_size];


float Max(float a[max])
{
        int i;
        float best=0;
        for(i=0; i<size; i++)
                if(a[i]>best)
                        best=a[i];
        return(best);
}

void meanfitness()
{
        int i;
        float f=0;
        for(i=0; i<size; i++)
                f=f+fit[i];
        mean=10*(bestfitness-(f/size));
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

int swap_array(int array[max],int a,int b)
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
                fit[i]=1/(k*log(sum));

        }

}


void normalizePopulation()
{
        int i;
        normal=0;
        for (i = 0; i < size; i++)
                normal=normal+fit[i];
        for (i=0; i<size; i++)
                fit[i]=fit[i]/normal;
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
        normalizePopulation();
        bestfitness=Max(fit);
        for(i=0; i<size; i++)
        {

                if(fit[i]==bestfitness)
                {
                        printf(CYN "");
                        bestFitIndex=i;
                }

        }
        copy(population[0].order,population[bestFitIndex].order);
        bestFitIndex=0;
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


int GMutation(int bestorder[max],float distance,int level)
{
        int i,j,order[max],current[max],c=0;
        float cd;
        copy(order,bestorder);
        copy(current,order);
        for(i=1; i<k-1; i++)
        {
                for(j=i+1; j<k-1; j++)
                {
                        swap_array(order,i,j);
                        d=calculatedistance(order);
                        cd=calculatedistance(current);
                        if(d<distance)
                        {
                                copy(bestorder,order);
                                if(c<level)
                                        c++;
                                else
                                        return 1;

                        }
                        else
                        {
                                if(c<level)
                                {
                                        c++;
                                        if(cd<d)
                                        {
                                                copy(current,order);
                                        }
                                        else
                                                copy(order,current);
                                }
                                else
                                        copy(order,bestorder);
                        }
                }

        }
        d=distance;
        return 0;
}


void mutate(int order_passed[max],float fitness)
{
        int r1,r2,i,order[max];
        int level,temp;
        float pd,d;
        d=calculatedistance(order_passed);
        copy(order,order_passed);
        meanfitness();
        r1=size*fitness;
        r2=size*mean;
        level=mutationrate*(abs(10*(r2-r1)));

        for ( i = 0; i < level; i++)
        {
                r1=random()%k;
                r1=random()%k;
                if(r1==0) r1=1;
                if(r2==0) r2=1;
                if(r1==k-1) r1=k-2;
                if(r2==k-1) r1=k-2;

                if (r1>0&&r1<k-1&&r2>0&&r2<k-1)
                {
                        temp=order[r1];
                        order[r1]=order[r2];
                        order[r2]=temp;
                }
                pd=calculatedistance(order);
                if(pd<d)
                        copy(order_passed,order);
                else
                        copy(order,order_passed);
        }
}



void evolve()
{
        int i,j,var,temp,p,order[max];
        // for(i=0; i<size; i++)
        //         if(dist(i)<bestDistance+10) ;
        //         else
        //                 copy(population[i].order,bestOrder);

        for(i=1; i<size; i++)
        {
                copy(order,population[i].order);
                for(var=rand()%k+1; var<k-1; var++)
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
                // temp=population[i].order[(i+1)];
                // population[i].order[(i+1)]=population[i].order[i];
                // population[i].order[i]=temp;
                // GMutation(population[i].order,dist(i),10);
        }
        for(j=0; j<evolutionrate; j++)
        {
                p=1;
                for(i=1; i<size; i++)
                {
                        var=random()%size;
                        if(var==0) var=1;
                        if(p<k-2)
                                swap(var,p,p+1);
                        p++;

                }

        }
        FitPopulation();
        normalizePopulation();
        SelectPopulation();
        generation++;
}

void bestdists()
{
        int i,j;
        float d;
        for(i=0; i<25; i++)
        {
                d=dist(i);

                printf(MAG "");
                for(j=0; j<k; j++)
                        printf("%d ",population[i].order[j]);
                printf(WHT "d:%f\n",d );

        }
        printf("\n");
}
void main()
{
        int i,j,m,track=0;
        printf("Number of Points:");
        scanf("%d",&m);
        time(&start_t);
        k=m+2;
        evolutionrate=2*k+(k/2);
        rateconst=pow(2,log10(k));
        Genpoints();
        Genpoints();
        // for(i=0; i<m; i++)
        // {
        //         printf("%d,",points[i].x);
        //         printf("%d|",points[i].y);
        //
        // }

        size=(k*100)%10000;
        verification_max=1000;
        GenPop();
        for(i=0; i<size; i++)
        {
                shuffle(population[i].order);
                // printf("\n" );
        }
        // exit(0);
        FitPopulation();
        normalizePopulation();
        SelectPopulation();
        printf(YEL "\nBest fitness: %f\tMean:%f\n",bestfitness,mean);
        verification_count=0;
        m=1;
        printf(CYN "Best Performance: %f\n",dist(bestFitIndex));
        while(verification_count<verification_max)
        {
                bestDistance=dist(bestFitIndex);
                prevDistance=bestDistance;

                for (j=0; j<rateconst; j++)
                {

                        for(i=0; i<size; i++)
                                if(i!=bestFitIndex)
                                        mutate(population[i].order,fit[i]);
                        FitPopulation();
                        normalizePopulation();
                        SelectPopulation();

                }

                system("clear");
                printf("\n" );
                viewpoints();
                printf("Total Points: %d\n",k);

                printf(BLU "Generation:%d\t",generation);
                printf(BLU "GMutation:%d\tTrack:%d\tD:%f\n",m,track,d );

                printf(YEL "\nBest fitness: %f\tMean:%f\n",bestfitness,mean);
                bestDistance=dist(bestFitIndex);
                printf(CYN "Best Performance: %f\t",bestDistance);
                printf(CYN "Time:%ds\n",(int)besttime);

                copy(bestOrder,population[0].order);
                printf(GRN "Best Sequence: ");
                for(i=0; i<k; i++)
                        printf("%d ",bestOrder[i] );
                printf("\n");

                if(prevDistance==bestDistance)
                {
                        verification_count++;
                        if(verification_count/100>mutationrate)
                        {
                                mutationrate++;
                                // size=size-100;
                        }
                }
                else
                {
                        verification_count=0;
                        mutationrate=1;
                        time(&end_t);
                        besttime = difftime(end_t, start_t);
                }
                if(mutationrate>=5)
                {
                        mutationrate=1;

                }

                printf(WHT "UC Check: %d%c\t",(100*verification_count/verification_max),'%' );
                time(&end_t);
                elapsedtime=difftime(end_t,start_t);
                printf(WHT "Time Elapsed:%dm %ds\n\n",(int)elapsedtime/60,(int)elapsedtime%60);
                // evolve();
                // track=track+GMutation(population[bestFitIndex].order,dist(bestFitIndex),11);

                bestdists();

                if(generation/10>m)
                {
                        track=track+GMutation(population[bestFitIndex].order,dist(bestFitIndex),20);

                        FitPopulation();
                        normalizePopulation();
                        SelectPopulation();
                        copy(bestOrder,population[bestFitIndex].order);
                        m++;

                }
                evolve();
        }
        FitPopulation();
        normalizePopulation();
        SelectPopulation();
        printf(YEL "\nBest fitness: %f\n",bestfitness);
        printf(CYN "Best Performance: %f\n",dist(bestFitIndex));
        printf(GRN "Best Sequence: ");
        for(i=0; i<k; i++)
                printf("%d ",bestOrder[i] );
        printf("\n");
        time(&end_t);
        elapsedtime=difftime(end_t,start_t);
        printf(CYN "Best Time:%ds\n",(int)besttime);
        printf(WHT "Time Elapsed:%dm %ds\n",(int)elapsedtime/60,(int)elapsedtime%60);
}

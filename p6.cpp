#include <iostream>

using namespace std;
int a,b=0,x;
int pantofelki(int a)
{
    if(a==0)
        return 0;
    if(a==1)
    {
        return 1;
    }
    else if(a==2)
    {
        return 1;
    }
    else
    {
return (pantofelki(a-1)+pantofelki(a-2));
    }


};

int main()
{


cin >> x;

for(int i =0 ; i< x ; i++) 
{
     cin>>a;       
   b=0; 
for(int i=a;i>0;i--)
{
b+=pantofelki(i);
}
cout<<b<<endl;
}


return 0;
};

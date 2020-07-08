//-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
// NASA Goddard Space Flight Center Land Information System (LIS) v7.2
//
// Copyright (c) 2015 United States Government as represented by the
// Administrator of the National Aeronautics and Space Administration.
// All Rights Reserved.
//-------------------------END NOTICE -- DO NOT EDIT-----------------------
//BOP
//
// !MODULE: LIS_glacierfrac_FTable
//  
//
// !DESCRIPTION:
//  Function table registries for storing the interface 
//  implementations for managing different sources of 
//  glacier fraction data
//   
//EOP
#include<stdio.h>
#include<stdlib.h>
#include<stdarg.h>
#include <string.h>

#include "ftn_drv.h"

struct glacierfracsetnode
{ 
  char *name;
  void (*func)(int*);

  struct glacierfracsetnode* next;
} ;
struct glacierfracsetnode* glacierfracset_table = NULL; 

struct glacierfracreadnode
{ 
  char *name;
  void (*func)(int*,void*, void*,float*,float*);

  struct glacierfracreadnode* next;
} ;
struct glacierfracreadnode* glacierfracread_table = NULL; 

//BOP
// !ROUTINE: registerglacierfracsetup
// \label{registerglacierfracsetup}
// 
// !INTERFACE:
void FTN(registerglacierfracsetup)(char *j,void (*func)(int*),int len)
// !DESCRIPTION: 
// Makes an entry in the registry for the routine to 
// setup glacierfrac data reading routines
//
// The arguments are: 
// \begin{description}
// \item[i]
//  index of the domain
//  \item[j]
//  index of the greenness data source
//  \end{description}
//EOP
{ 
  int len1;
  struct glacierfracsetnode* current;
  struct glacierfracsetnode* pnode; 
  // create node
  
  len1 = len + 1; // ensure that there is space for terminating null
  pnode=(struct glacierfracsetnode*) malloc(sizeof(struct glacierfracsetnode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL; 

  if(glacierfracset_table == NULL){
    glacierfracset_table = pnode;
  }
  else{
    current = glacierfracset_table; 
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode; 
  }

}

//BOP
// !ROUTINE: glacierfracsetup
// \label{glacierfracsetup}
//
// !INTERFACE:
void FTN(glacierfracsetup)(char *j,int *n, int len)
//  
// !DESCRIPTION:
// Invokes the routine from the registry to 
// setup glacierfrac data reading
// 
// The arguments are: 
// \begin{description}
//  \item[n]
//   index of the nest
//  \item[j]
//  index of the greenness data source
//  \end{description}
//EOP
{ 
  struct glacierfracsetnode* current;
  
  current = glacierfracset_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("glacierfracsetup routine for runmode %s is not defined\n",j); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }
  }
  current->func(n); 
}

//BOP
// !ROUTINE: registerreadglacierfrac
// \label{registerreadglacierfrac}
// 
// !INTERFACE:
void FTN(registerreadglacierfrac)(char *j,void (*func)(int*, void*, void*, float*, float*),int len)
// !DESCRIPTION: 
// Makes an entry in the registry for the routine to 
// read glacierfrac data
//
// The arguments are: 
// \begin{description}
// \item[i]
//  index of the domain
//  \item[j]
//  index of the greenness data source
//  \end{description}
//EOP
{ 
  int len1;
  struct glacierfracreadnode* current;
  struct glacierfracreadnode* pnode; 
  // create node
  
  len1 = len + 1; // ensure that there is space for terminating null
  pnode=(struct glacierfracreadnode*) malloc(sizeof(struct glacierfracreadnode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL; 

  if(glacierfracread_table == NULL){
    glacierfracread_table = pnode;
  }
  else{
    current = glacierfracread_table; 
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode; 
  }

}

//BOP
// !ROUTINE: readglacierfrac
// \label{readglacierfrac}
//
// !INTERFACE:
void FTN(readglacierfrac)(char *j,int *n, void *time1, void *time2, float *array1, float *array2, int len)
//  
// !DESCRIPTION:
// Invokes the routine from the registry to 
// reading glacierfrac data 
// 
// The arguments are: 
// \begin{description}
//  \item[n]
//   index of the nest
//  \item[j]
//  index of the greenness data source
//  \item[time]
//  month 
//  \item[array]
//  pointer to the greenness data
//  \end{description}
//EOP
{ 
  struct glacierfracreadnode* current;
  
  current = glacierfracread_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("readglacierfrac routine for runmode %s is not defined\n",j); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }
  }
  current->func(n,time1,time2,array1,array2); 
}




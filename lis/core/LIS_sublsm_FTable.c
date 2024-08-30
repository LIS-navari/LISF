//-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
// NASA Goddard Space Flight Center
// Land Information System Framework (LISF)
// Version 7.5
//
// Copyright (c) 2024 United States Government as represented by the
// Administrator of the National Aeronautics and Space Administration.
// All Rights Reserved.
//-------------------------END NOTICE -- DO NOT EDIT-----------------------
//BOP
//
// !MODULE: LIS_sublsm_FTable
//  
//
// !DESCRIPTION:
//  Function table registries for storing the interface 
//  implementations for managing the operations of different 
//  land surface models. The registries also contain 
//  related interface implementations for data assimilation, 
//  WRF/GCE/GFS coupling and parameter estimation
//
//EOP
#include<stdio.h>
#include<stdlib.h>
#include<stdarg.h>
#include<string.h>

#include "ftn_drv.h"
struct sublsminitnode
{ 
  char *name;
  void (*func)(int*);

  struct sublsminitnode* next;
} ;
struct sublsminitnode* sublsminit_table = NULL; 

struct sublsmrunnode
{ 
  char *name;
  void (*func)(int*);

  struct sublsmrunnode* next;
} ;
struct sublsmrunnode* sublsmrun_table = NULL; 

struct sublsmfinalnode
{ 
  char *name;
  void (*func)();

  struct sublsmfinalnode* next;
} ;
struct sublsmfinalnode* sublsmfinal_table = NULL; 

struct sublsmresetnode
{ 
  char *name;
  void (*func)();

  struct sublsmresetnode* next;
} ;
struct sublsmresetnode* sublsmreset_table = NULL; 

struct sublsmsetupnode
{ 
  char *name;
  void (*func)();

  struct sublsmsetupnode* next;
} ;
struct sublsmsetupnode* sublsmsetup_table = NULL;

struct sublsmrestartnode
{ 
  char *name;
  void (*func)();

  struct sublsmrestartnode* next;
} ;
struct sublsmrestartnode* sublsmrestart_table = NULL;


struct sublsmdynsetnode
{ 
  char *name;
  void (*func)(int*);

  struct sublsmdynsetnode* next;
} ;
struct sublsmdynsetnode* sublsmdynset_table = NULL;

struct sublsmf2tnode
{ 
  char *name;
  void (*func)(int*);

  struct sublsmf2tnode* next;
} ;
struct sublsmf2tnode* sublsmf2t_table = NULL;

struct sublsmwriterstnode
{ 
  char *name;
  void (*func)(int*);

  struct sublsmwriterstnode* next;
} ;
struct sublsmwriterstnode* sublsmwriterst_table = NULL;

struct lsm2sublsmgetexportnode
{ 
  char *name;
  void (*func)(int*, void*);

  struct lsm2sublsmgetexportnode* next;
} ;
struct lsm2sublsmgetexportnode* lsm2sublsmgetexport_table = NULL;

struct sublsmsetlsmimportnode
{ 
  char *name;
  void (*func)(int*, void*);

  struct sublsmsetlsmimportnode* next;
} ;
struct sublsmsetlsmimportnode* sublsmsetlsmimport_table = NULL;

struct sublsm2lsmgetexportnode
{ 
  char *name;
  void (*func)(int*, void*);

  struct sublsm2lsmgetexportnode* next;
} ;
struct sublsm2lsmgetexportnode* sublsm2lsmgetexport_table = NULL;

struct lsmsetsublsmimportnode
{ 
  char *name;
  void (*func)(int*, void*);

  struct lsmsetsublsmimportnode* next;
} ;
struct lsmsetsublsmimportnode* lsmsetsublsmimport_table = NULL;

//for DA
struct sublsmdainitnode
{
  char *name;
  void (*func)(int*);

  struct sublsmdainitnode* next;
} ;
struct sublsmdainitnode* sublsmdainit_table = NULL;

struct sublsmdagetvarnode
{
  char *name;
  void (*func)(int*, void*);

  struct sublsmdagetvarnode* next;
} ;
struct sublsmdagetvarnode* sublsmdagetvar_table = NULL;

struct sublsmdasetvarnode
{
  char *name;
  void (*func)(int*, void*);

  struct sublsmdasetvarnode* next;
} ;
struct sublsmdasetvarnode* sublsmdasetvar_table = NULL;

struct sublsmdasetPWnode
{
  char *name;
  void (*func)(int*, void*);

  struct sublsmdasetPWnode* next;
} ;
struct sublsmdasetPWnode* sublsmdasetPW_table = NULL;

struct sublsmdaobstransformnode
{
  char *name;
  void (*func)(int*, void*);

  struct sublsmdaobstransformnode* next;
} ;
struct sublsmdaobstransformnode* sublsmdaobstransform_table = NULL;

struct sublsmdamapobstosublsmnode
{
  char *name;
  void (*func)(int*, int*, void*,void*);

  struct sublsmdamapobstosublsmnode* next;
} ;
struct sublsmdamapobstosublsmnode* sublsmdamapobstosublsm_table = NULL;

struct sublsmdaobsprednode
{
  char *name;
  void (*func)(int*, int*, float*);

  struct sublsmdaobsprednode* next;
} ;
struct sublsmdaobsprednode* sublsmdaobspred_table = NULL;

struct sublsmdaqcstatenode
{
  char *name;
  void (*func)(int*, void*);

  struct sublsmdaqcstatenode* next;
} ;
struct sublsmdaqcstatenode* sublsmdaqcstate_table = NULL;

struct sublsmdaqcobsnode
{
  char *name;
  void (*func)(int*, int*, void*);

  struct sublsmdaqcobsnode* next;
} ;
struct sublsmdaqcobsnode* sublsmdaqcobs_table = NULL;

struct sublsmdascalenode
{
  char *name;
  void (*func)(int*, void*);

  struct sublsmdascalenode* next;
} ;
struct sublsmdascalenode* sublsmdascale_table = NULL;

struct sublsmdadescalenode
{
  char *name;
  void (*func)(int*, void*, void*);

  struct sublsmdadescalenode* next;
} ;
struct sublsmdadescalenode* sublsmdadescale_table = NULL;

struct sublsmdaupdatenode
{
  char *name;
  void (*func)(int*, void*,void*);

  struct sublsmdaupdatenode* next;
} ;
struct sublsmdaupdatenode* sublsmdaupdate_table = NULL;

struct sublsmdawritevarnode
{
  char *name;
  void (*func)(int*, int*,void*);

  struct sublsmdawritevarnode* next;
} ;
struct sublsmdawritevarnode* sublsmdawritevar_table = NULL;

struct sublsmdiagfordanode
{
  char *name;
  void (*func)(int*);

  struct sublsmdiagfordanode* next;
} ;
struct sublsmdiagfordanode* sublsmdiagforda_table = NULL;

//BOP
// !ROUTINE: registersublsmini
// \label{registersublsmini}
//  
// !DESCRIPTION: 
//  Creates an entry in the registry for the routine to 
//  perform land surface model initialization
// 
// !INTERFACE:
void FTN(registersublsminit)(char *j, void (*func)(int*),int len)
//EOP
{ 
  int len1;
  struct sublsminitnode* current;
  struct sublsminitnode* pnode; 
  // create node
  
  len1 = len + 1; // ensure that there is space for terminating null
  pnode=(struct sublsminitnode*) malloc(sizeof(struct sublsminitnode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL; 

  if(sublsminit_table == NULL){
    sublsminit_table = pnode;
  }
  else{
    current = sublsminit_table; 
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode; 
  }
}
//BOP
// !ROUTINE: sublsminit
// \label{sublsminit}
//
// !INTERFACE:
void FTN(sublsminit)(char *j,int *kk, int len)
//
//  
// !DESCRIPTION: 
//  Invokes the routine from the registry to perform
//  land surface model initialization
// 
//  \begin{description}
//  \item[j]
//   name of the SUBLSM
//  \item[kk] 
//   index of the SUBLSM
//  \end{description}
//EOP
{ 

  struct sublsminitnode* current;
  int found ; 

  current = sublsminit_table;

  while(strcmp(current->name,j)!=0){
    current = current->next;

    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("init routine for SUBLSM %s is not defined\n",j); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }
  }
  current->func(kk); 

}
//BOP
// !ROUTINE: registersublsmrun
// \label{registersublsmrun}
//
// !INTERFACE:
void FTN(registersublsmrun)(char *j, void (*func)(int*),int len)
//  
// !DESCRIPTION: 
//  Creates an entry in the registry for the routine
//  to run the land surface model 
// 
//  \begin{description}
//  \item[j]
//   name of the SUBLSM
//  \end{description}
//EOP
{ 
  int len1;
  struct sublsmrunnode* current;
  struct sublsmrunnode* pnode; 
  // create node
  
  len1 = len + 1; // ensure that there is space for terminating null
  pnode=(struct sublsmrunnode*) malloc(sizeof(struct sublsmrunnode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL; 

  if(sublsmrun_table == NULL){
    sublsmrun_table = pnode;
  }
  else{
    current = sublsmrun_table; 
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode; 
  }

}
//BOP
// !ROUTINE: sublsmrun
// \label{sublsmrun}
//
// !INTERFACE:
void FTN(sublsmrun)(char *j,int *n,int len)
//  
// !DESCRIPTION: 
//  Invokes the routine from the registry to run the 
//  land surface model 
// 
//  \begin{description}
//  \item[j]
//   name of the SUBLSM
//  \item[n]
//   index of the nest
//  \end{description}
//EOP
{
  struct sublsmrunnode* current;
  
  current = sublsmrun_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;  
    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("run routine for SUBLSM %s is not defined\n",j); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }
  }
  current->func(n); 
}

//BOP
// !ROUTINE: registersublsmfinalize
// \label{registersublsmfinalize}
//
// !INTERFACE:
void FTN(registersublsmfinalize)(char *j, void (*func)(),int len)
//  
// !DESCRIPTION:
//  Creates an entry in the registry for the routine
//  to cleanup allocated structures specific to the 
//  land surface model
// 
//  \begin{description}
//  \item[j]
//   name of the SUBLSM
//  \end{description}
// 
//EOP
{ 
  int len1;
  struct sublsmfinalnode* current;
  struct sublsmfinalnode* pnode; 
  // create node
  
  len1 = len + 1; // ensure that there is space for terminating null
  pnode=(struct sublsmfinalnode*) malloc(sizeof(struct sublsmfinalnode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL; 

  if(sublsmfinal_table == NULL){
    sublsmfinal_table = pnode;
  }
  else{
    current = sublsmfinal_table; 
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode; 
  }
}
//BOP
// !ROUTINE: sublsmfinalize
// \label{sublsmfinalize}
//
// !INTERFACE:
void FTN(sublsmfinalize)(char *j,int len)
//  
// !DESCRIPTION: 
//  Invokes the routine from the registry for cleaning up
//  allocated structures specific to the land surface model
// 
//  \begin{description}
//  \item[j]
//   name of the SUBLSM
//  \end{description}
// 
//EOP
{
  struct sublsmfinalnode* current;
  
  current = sublsmfinal_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("finalize routine for SUBLSM %s is not defined\n",j); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }
  }
  current->func(); 
}

//BOP
// !ROUTINE: registersublsmreset
// \label{registersublsmreset}
//
// !INTERFACE:
void FTN(registersublsmreset)(char *j, void (*func)(),int len)
//  
// !DESCRIPTION:
//  Creates an entry in the registry for the routine
//  to cleanup allocated structures specific to the 
//  land surface model
// 
//  \begin{description}
//  \item[j]
//   name of the LSM
//  \end{description}
// 
//EOP
{
  int len1;
  struct sublsmresetnode* current;
  struct sublsmresetnode* pnode;
  // create node

  len1 = len + 1; // ensure that there is space for terminating null
  pnode=(struct sublsmresetnode*) malloc(sizeof(struct sublsmresetnode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL;

  if(sublsmreset_table == NULL){
    sublsmreset_table = pnode;
  }
  else{
    current = sublsmreset_table;
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode;
  }
}
//BOP
// !ROUTINE: sublsmreset
// \label{sublsmreset}
//
// !INTERFACE:
void FTN(sublsmreset)(char *j,int len)
//  
// !DESCRIPTION: 
//  Invokes the routine from the registry for cleaning up
//  allocated structures specific to the land surface model
// 
//  \begin{description}
//  \item[j]
//   name of the LSM
//  \end{description}
// 
//EOP
{
  struct sublsmresetnode* current;

  current = sublsmreset_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n");
      printf("reset routine for LSM %s is not defined\n",j);
      printf("program will seg fault.....\n");
      printf("****************Error****************************\n");
    }
  }
  current->func();
}

//BOP
// !ROUTINE: registersublsmsetup
// \label{registersublsmsetup}
//
// !INTERFACE:
void FTN(registersublsmsetup)(char *j, void (*func)(),int len)
//  
// !DESCRIPTION:
//  Makes an entry in the registry for the routine
//  to set up land surface model parameters 
// 
//  \begin{description}
//  \item[j]
//   name of the SUBLSM
//  \end{description}
//EOP
{ 
  int len1;
  struct sublsmsetupnode* current;
  struct sublsmsetupnode* pnode; 
  // create node
  
  len1 = len + 1; // ensure that there is space for terminating null
  pnode=(struct sublsmsetupnode*) malloc(sizeof(struct sublsmsetupnode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL; 

  if(sublsmsetup_table == NULL){
    sublsmsetup_table = pnode;
  }
  else{
    current = sublsmsetup_table; 
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode; 
  }

}
//BOP
// !ROUTINE: sublsmsetup
// \label{sublsmsetup}
//
// !INTERFACE:
void FTN(sublsmsetup)(char *j, int len)
//  
// !DESCRIPTION:  
//  Invokes the routine in the registry to set up 
//  land surface model parameters  
// 
//  \begin{description}
//  \item[j]
//   name of the SUBLSM
//  \end{description}
//EOP
{ 
  struct sublsmsetupnode* current;
  
  current = sublsmsetup_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    
    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("setup routine for SUBLSM %s is not defined\n",j); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }
  }
  current->func(); 
}

//BOP
// !ROUTINE: registersublsmrestart
// \label{registersublsmrestart}
// 
// !INTERFACE:
void FTN(registersublsmrestart)(char *j, void (*func)(),int len)
//  
// !DESCRIPTION: 
// Makes an entry in the registry for the routine to 
// restart the land surface model from a 
// previously saved state
// 
//  \begin{description}
//  \item[j]
//   name of the SUBLSM
//  \end{description}
//EOP
{ 
  int len1;
  struct sublsmrestartnode* current;
  struct sublsmrestartnode* pnode; 
  // create node
  
  len1 = len + 1; // ensure that there is space for terminating null
  pnode=(struct sublsmrestartnode*) malloc(sizeof(struct sublsmrestartnode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL; 

  if(sublsmrestart_table == NULL){
    sublsmrestart_table = pnode;
  }
  else{
    current = sublsmrestart_table; 
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode; 
  }

}
//BOP
// !ROUTINE: sublsmrestart
// \label{sublsmrestart}
//
// !INTERFACE:
void FTN(sublsmrestart)(char *j, int len)
//  
// !DESCRIPTION: 
//  Invokes the routine from the registry to 
//  restart the land surface model from a previously
//  saved state
// 
//  \begin{description}
//  \item[j]
//   name of the SUBLSM
//  \end{description}
//EOP
{ 
  struct sublsmrestartnode* current;
  
  current = sublsmrestart_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("read restart routine for SUBLSM %s is not defined\n",j); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }
  }
  current->func(); 
}

//BOP
// !ROUTINE: registersublsmdynsetup
// \label{registersublsmdynsetup}
// 
// !INTERFACE:
void FTN(registersublsmdynsetup)(char *j, void (*func)(int*),int len)
//  
// !DESCRIPTION: 
//  Makes an entry in the registry for the routine to 
//  set the time dependent land surface parameters
// 
//  \begin{description}
//  \item[j]
//   name of the SUBLSM
//  \end{description}
//EOP
{ 
  int len1;
  struct sublsmdynsetnode* current;
  struct sublsmdynsetnode* pnode; 
  // create node
  
  len1 = len + 1; // ensure that there is space for terminating null
  pnode=(struct sublsmdynsetnode*) malloc(sizeof(struct sublsmdynsetnode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL; 

  if(sublsmdynset_table == NULL){
    sublsmdynset_table = pnode;
  }
  else{
    current = sublsmdynset_table; 
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode; 
  }

}
//BOP
// !ROUTINE: sublsmdynsetup
// \label{sublsmdynsetup}
// 
// !INTERFACE:
void FTN(sublsmdynsetup)(char *j, int *n, int len)
//  
// !DESCRIPTION: 
//  Invokes the routine from the registry to set the 
//  time dependent land surface parameters
// 
//  \begin{description}
//  \item[j]
//   name of the SUBLSM
//  \item[n]
//   index of the nest
//  \end{description}
//EOP
{ 
  struct sublsmdynsetnode* current;
  
  current = sublsmdynset_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;

    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("dynamic setup routine for SUBLSM %s is not defined\n",j); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }
  }
  current->func(n); 
}


//BOP
// !ROUTINE: registersublsmf2t
// \label{registersublsmf2t}
// 
// !INTERFACE:
void FTN(registersublsmf2t)(char *j, void (*func)(int*),int len)
//  
// !DESCRIPTION: 
//  Makes an entry in the registry for the routine to 
//  transfer forcing to model tiles
// 
//  \begin{description}
//  \item[j]
//   name of the SUBLSM + name of the running mode
//  \item[j]
//   index of the runmode
//  \end{description}
//EOP
{ 
  int len1;
  struct sublsmf2tnode* current;
  struct sublsmf2tnode* pnode; 
  // create node
  
  len1 = len + 1; // ensure that there is space for terminating null
  pnode=(struct sublsmf2tnode*) malloc(sizeof(struct sublsmf2tnode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL; 

  if(sublsmf2t_table == NULL){
    sublsmf2t_table = pnode;
  }
  else{
    current = sublsmf2t_table; 
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode; 
  }

}
//BOP
// !ROUTINE: sublsmf2t
// \label{sublsmf2t}
// 
// !INTERFACE:
void FTN(sublsmf2t)(char *j, int *n, int len)
//  
// !DESCRIPTION: 
//  Invokes the routine from the registry to 
//  transfer forcing to model tiles
// 
//  \begin{description}
//  \item[j]
//   name of the SUBLSM
//  \item[j]
//   index of the runmode
//  \item[n]
//   index of the nest
//  \end{description}
//EOP
{ 
  struct sublsmf2tnode* current;
  
  current = sublsmf2t_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;

    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("f2t writing routine for SUBLSM and running mode %s is not defined\n",j); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }
  }
  current->func(n); 
}

//BOP
// !ROUTINE: registersublsmwrst
// \label{registersublsmwrst}
// 
// !INTERFACE:
void FTN(registersublsmwrst)(char *j, void (*func)(int*),int len)
//  
// !DESCRIPTION: 
//  Makes an entry in the registry for the routine 
//  to write restart files for a land surface model 
// 
//  \begin{description}
//  \item[j]
//   name of the SUBLSM
//  \end{description}
//EOP
{ 
  int len1;
  struct sublsmwriterstnode* current;
  struct sublsmwriterstnode* pnode; 
  // create node
  
  len1 = len + 1; // ensure that there is space for terminating null
  pnode=(struct sublsmwriterstnode*) malloc(sizeof(struct sublsmwriterstnode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL; 

  if(sublsmwriterst_table == NULL){
    sublsmwriterst_table = pnode;
  }
  else{
    current = sublsmwriterst_table; 
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode; 
  }
}
//BOP
// !ROUTINE: sublsmwrst
// \label{sublsmwrst}
// 
// !INTERFACE:
void FTN(sublsmwrst)(char *j, int *n, int len)
//  
// !DESCRIPTION:
//  Invokes the routine from the registry to write
//  restart files for a land surface model
// 
//  \begin{description}
//  \item[j]
//   name of the SUBLSM
//  \item[n]
//   index of the nest
//  \end{description}
//EOP
{ 

  struct sublsmwriterstnode* current;
  
  current = sublsmwriterst_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("write restart writing routine for SUBLSM %s is not defined\n",j); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }
  }
  current->func(n); 
}

//BOP
// !ROUTINE: registerlsm2sublsmgetexport
// \label{registerlsm2sublsmgetexport}
// 
// !INTERFACE:
void FTN(registerlsm2sublsmgetexport)(char *j, void (*func)(int*, void*),int len)
//  
// !DESCRIPTION: 
//  Makes an entry in the registry for the routine 
//  to write restart files for a land surface model 
// 
//  \begin{description}
//  \item[j]
//   name of the SUBLSM
//  \end{description}
//EOP
{ 
  int len1;
  struct lsm2sublsmgetexportnode* current;
  struct lsm2sublsmgetexportnode* pnode; 
  // create node
  
  len1 = len + 1; // ensure that there is space for terminating null
  pnode=(struct lsm2sublsmgetexportnode*) malloc(sizeof(struct lsm2sublsmgetexportnode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL; 

  if(lsm2sublsmgetexport_table == NULL){
    lsm2sublsmgetexport_table = pnode;
  }
  else{
    current = lsm2sublsmgetexport_table; 
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode; 
  }
}
//BOP
// !ROUTINE: lsm2sublsmgetexport
// \label{lsm2sublsmgetexport}
// 
// !INTERFACE:
void FTN(lsm2sublsmgetexport)(char *j, int *n, void *state, int len)
//  
// !DESCRIPTION:
//  Invokes the routine from the registry to write
//  restart files for a land surface model
// 
//  \begin{description}
//  \item[j]
//   name of the SUBLSM
//  \item[n]
//   index of the nest
//  \end{description}
//EOP
{ 

  struct lsm2sublsmgetexportnode* current;
  
  current = lsm2sublsmgetexport_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("get export routine for LSM2SUBLSM %s is not defined\n",j); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }
  }
  current->func(n, state); 
}

//BOP
// !ROUTINE: registersublsmsetlsmimport
// \label{registersublsmsetlsmimport}
// 
// !INTERFACE:
void FTN(registersublsmsetlsmimport)(char *j, void (*func)(int*, void*),int len)
//  
// !DESCRIPTION: 
//  Makes an entry in the registry for the routine 
//  to write restart files for a land surface model 
// 
//  \begin{description}
//  \item[j]
//   name of the SUBLSM
//  \end{description}
//EOP
{ 
  int len1;
  struct sublsmsetlsmimportnode* current;
  struct sublsmsetlsmimportnode* pnode; 
  // create node
  
  len1 = len + 1; // ensure that there is space for terminating null
  pnode=(struct sublsmsetlsmimportnode*) malloc(sizeof(struct sublsmsetlsmimportnode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL; 

  if(sublsmsetlsmimport_table == NULL){
    sublsmsetlsmimport_table = pnode;
  }
  else{
    current = sublsmsetlsmimport_table; 
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode; 
  }
}
//BOP
// !ROUTINE: sublsmsetlsmimport
// \label{sublsmsetlsmimport}
// 
// !INTERFACE:
void FTN(sublsmsetlsmimport)(char *j, int *n, void *state,int len)
//  
// !DESCRIPTION:
//  Invokes the routine from the registry to write
//  restart files for a land surface model
// 
//  \begin{description}
//  \item[j]
//   name of the SUBLSM
//  \item[n]
//   index of the nest
//  \end{description}
//EOP
{ 

  struct sublsmsetlsmimportnode* current;
  
  current = sublsmsetlsmimport_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("set export routine for LSM2SUBLSM %s is not defined\n",j); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }
  }
  current->func(n,state); 
}

//BOP
// !ROUTINE: registersublsm2lsmgetexport
// \label{registersublsm2lsmgetexport}
// 
// !INTERFACE:
void FTN(registersublsm2lsmgetexport)(char *j, void (*func)(int*, void*),int len)
//  
// !DESCRIPTION: 
//  Makes an entry in the registry for the routine 
//  to write restart files for a land surface model 
// 
//  \begin{description}
//  \item[j]
//   name of the SUBLSM
//  \end{description}
//EOP
{ 
  int len1;
  struct sublsm2lsmgetexportnode* current;
  struct sublsm2lsmgetexportnode* pnode; 
  // create node
  
  len1 = len + 1; // ensure that there is space for terminating null
  pnode=(struct sublsm2lsmgetexportnode*) malloc(sizeof(struct sublsm2lsmgetexportnode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL; 

  if(sublsm2lsmgetexport_table == NULL){
    sublsm2lsmgetexport_table = pnode;
  }
  else{
    current = sublsm2lsmgetexport_table; 
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode; 
  }
}
//BOP
// !ROUTINE: sublsm2lsmgetexport
// \label{sublsm2lsmgetexport}
// 
// !INTERFACE:
void FTN(sublsm2lsmgetexport)(char *j, int *n, void *state,int len)
//  
// !DESCRIPTION:
//  Invokes the routine from the registry to write
//  restart files for a land surface model
// 
//  \begin{description}
//  \item[j]
//   name of the SUBLSM
//  \item[n]
//   index of the nest
//  \end{description}
//EOP
{ 

  struct sublsm2lsmgetexportnode* current;
  
  current = sublsm2lsmgetexport_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("get export routine for SUBLSM2LSM %s is not defined\n",j); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }
  }
  current->func(n, state); 
}

//BOP
// !ROUTINE: registerlsmsetsublsmimport
// \label{registerlsmsetsublsmimport}
// 
// !INTERFACE:
void FTN(registerlsmsetsublsmimport)(char *j, void (*func)(int*, void*),int len)
//  
// !DESCRIPTION: 
//  Makes an entry in the registry for the routine 
//  to write restart files for a land surface model 
// 
//  \begin{description}
//  \item[j]
//   name of the SUBLSM
//  \end{description}
//EOP
{ 
  int len1;
  struct lsmsetsublsmimportnode* current;
  struct lsmsetsublsmimportnode* pnode; 
  // create node
  
  len1 = len + 1; // ensure that there is space for terminating null
  pnode=(struct lsmsetsublsmimportnode*) malloc(sizeof(struct lsmsetsublsmimportnode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL; 

  if(lsmsetsublsmimport_table == NULL){
    lsmsetsublsmimport_table = pnode;
  }
  else{
    current = lsmsetsublsmimport_table; 
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode; 
  }
}
//BOP
// !ROUTINE: lsmsetsublsmimport
// \label{lsmsetsublsmimport}
// 
// !INTERFACE:
void FTN(lsmsetsublsmimport)(char *j, int *n, void *state, int len)
//  
// !DESCRIPTION:
//  Invokes the routine from the registry to write
//  restart files for a land surface model
// 
//  \begin{description}
//  \item[j]
//   name of the SUBLSM
//  \item[n]
//   index of the nest
//  \end{description}
//EOP
{ 

  struct lsmsetsublsmimportnode* current;
  
  current = lsmsetsublsmimport_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("setsublsmimport routine for SUBLSM2LSM %s is not defined\n",j); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }
  }
  current->func(n, state); 
}
//BOP
// !ROUTINE: registersublsmdainit
// \label{registersublsmdainit}
// 
// !INTERFACE:
void FTN(registersublsmdainit)(char *j, void (*func)(int*),int len)
//  
// !DESCRIPTION: 
//  Makes an entry in the registry for the routine 
//  for initializing DA related LSM settings
// 
//  \begin{description}
//  \item[j]
//   name of the SubLSM + DA instance
//  \end{description}
//EOP
{
  int len1;
  struct sublsmdainitnode* current;
  struct sublsmdainitnode* pnode;
  // create node

  len1 = len + 1; // ensure that there is space for terminating null
  pnode=(struct sublsmdainitnode*) malloc(sizeof(struct sublsmdainitnode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL;

  if(sublsmdainit_table == NULL){
    sublsmdainit_table = pnode;
  }
  else{
    current = sublsmdainit_table;
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode;
  }
}
//BOP
// !ROUTINE: sublsmdainit
// \label{sublsmdainit}
// 
// !INTERFACE:
void FTN(sublsmdainit)(char *j, int *k, int len)
//  
// !DESCRIPTION: 
//  Invokes the routine from the registry for initializing
//  DA related LSM settings. 
//  \begin{description}
//  \item[j]
//   name of the SubLSM + DA instance
//  \end{description}
//EOP
{
  struct sublsmdainitnode* current;

  current = sublsmdainit_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n");
      printf("init routine for SubLSM + DA instance %s is not defined\n",j);
      printf("program will seg fault.....\n");
      printf("****************Error****************************\n");
    }
  }
  current->func(k);
}
//BOP
// !ROUTINE: registersublsmdagetstatevar
// \label{registersublsmdagetstatevar}
// 
// !INTERFACE:
void FTN(registersublsmdagetstatevar)(char *j, void (*func)(int*, void*),int len)
//  
// !DESCRIPTION: 
//  Makes an entry in the registry for the routine 
//  for obtaining the specified prognostic variables from the 
//  land surface model (for data assimilation)
// 
//  \begin{description}
//  \item[j]
//   name of the SubLSM + DA instance
//  \end{description}
//EOP
{
  int len1;
  struct sublsmdagetvarnode* current;
  struct sublsmdagetvarnode* pnode;
  // create node

  len1 = len + 1; // ensure that there is space for terminating null
  pnode=(struct sublsmdagetvarnode*) malloc(sizeof(struct sublsmdagetvarnode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL;

  if(sublsmdagetvar_table == NULL){
    sublsmdagetvar_table = pnode;
  }
  else{
    current = sublsmdagetvar_table;
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode;
  }
}
//BOP
// !ROUTINE: sublsmdagetstatevar
// \label{sublsmdagetstatevar}
// 
// !INTERFACE:
void FTN(sublsmdagetstatevar)(char *j, int *n, void *state, int len)
//  
// !DESCRIPTION: 
//  Invokes the routine from the registry for obtaining
//  the specified prognostic variables from the land surface model
//  (for data assimilation)
// 
//  \begin{description}
//  \item[j]
//   name of the SubLSM + DA instance
//  \item[n]
//   index of the nest
//  \item[state]
//   pointer to the prognostic variable state
//  \end{description}
//EOP
{
  struct sublsmdagetvarnode* current;

  current = sublsmdagetvar_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n");
      printf("get LSM variable routine for SubLSM + DA instance %s is not defined\n",j);
      printf("program will seg fault.....\n");
      printf("****************Error****************************\n");
    }
  }
  current->func(n,state);
}

//BOP
// !ROUTINE: registersublsmdasetstatevar
// \label{registersublsmdasetstatevar}
// 
// !INTERFACE:
void FTN(registersublsmdasetstatevar)(char *j, void (*func)(int*, void*),int len)
//  
// !DESCRIPTION:
//  Makes an entry in the registry for updating the specified
//  state variable in a land surface model 
//  (for data assimilation)
// 
//  \begin{description}
//  \item[j]
//   name of the SubLSM + DA instance
//  \end{description}
//EOP
{
  int len1;
  struct sublsmdasetvarnode* current;
  struct sublsmdasetvarnode* pnode;
  // create node

  len1 = len + 1; // ensure that there is space for terminating null
  pnode=(struct sublsmdasetvarnode*) malloc(sizeof(struct sublsmdasetvarnode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL;

  if(sublsmdasetvar_table == NULL){
    sublsmdasetvar_table = pnode;
  }
  else{
    current = sublsmdasetvar_table;
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode;
  }
}
//BOP
// !ROUTINE: sublsmdasetstatevar
// \label{sublsmdasetstatevar}
// 
// !INTERFACE:
void FTN(sublsmdasetstatevar)(char *j,int *n, void *statevar, int len)
//  
// !DESCRIPTION: 
//  Invokes the routine from the registry for updating
//  the specified state variable in a land surface model 
//  (for data assimilation)
// 
//  \begin{description}
//  \item[j]
//   name of the SubLSM + DA instance
//  \item[n]
//   index of the nest
//  \item[statevars]
//   pointer to the prognostic variable state
//  \end{description}
//EOP
{
  struct sublsmdasetvarnode* current;

  current = sublsmdasetvar_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;

    if(current==NULL) {
      printf("****************Error****************************\n");
      printf("set LSM variable routine for SubLSM + DA instance %s is not defined\n",j);
      printf("program will seg fault.....\n");
      printf("****************Error****************************\n");
    }
  }
  current->func(n,statevar);
}

//BOP
// !ROUTINE: registersublsmdasetparticleweight
// \label{registersublsmdasetparticleweight}
// 
// !INTERFACE:
void FTN(registersublsmdasetparticleweight)(char *j, void (*func)(int*, void*),int len)
//  
// !DESCRIPTION:
//  Makes an entry in the registry for updating the specified
//  state variable in a land surface model 
//  (for data assimilation)
// 
//  \begin{description}
//  \item[j]
//   name of the SubLSM + DA instance
//  \end{description}
//EOP
{
  int len1;
  struct sublsmdasetPWnode* current;
  struct sublsmdasetPWnode* pnode;
  // create node

  len1 = len + 1; // ensure that there is space for terminating null
//  pnode=(struct sublsmdasetPWnode*) malloc(sizeof(struct sublsmdasetPWnode));
  pnode=(struct sublsmdasetPWnode*) malloc(sizeof(struct sublsmdasetPWnode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL;

  if(sublsmdasetPW_table == NULL){
    sublsmdasetPW_table = pnode;
  }
  else{
    current = sublsmdasetPW_table;
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode;
  }
}
//BOP
// !ROUTINE: sublsmdasetparticleweight
// \label{sublsmdasetparticleweight}
// 
// !INTERFACE:
void FTN(sublsmdasetparticleweight)(char *j,int *n, void *particleweight, int len)
//  
// !DESCRIPTION: 
//  Invokes the routine from the registry for updating
//  the specified state variable in a land surface model 
//  (for data assimilation)
// 
//  \begin{description}
//  \item[j]
//   name of the SubLSM + DA instance
//  \item[n]
//   index of the nest
//  \item[particleweight]
//   pointer to the prognostic variable state
//  \end{description}
//EOP
{
  struct sublsmdasetPWnode* current;

  current = sublsmdasetPW_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;

    if(current==NULL) {
      printf("****************Error****************************\n");
      printf("set LSM variable routine for SubLSM + DA instance %s is not defined\n",j);
      printf("program will seg fault.....\n");
      printf("****************Error****************************\n");
    }
  }
  current->func(n,particleweight);
}

//BOP
// !ROUTINE: registersublsmdaobstransform
// \label{registersublsmdaobstransform}
// 
// !INTERFACE:
void FTN(registersublsmdaobstransform)(char *j, void (*func)(int*, void*),int len)
//  
// !DESCRIPTION: 
//  Makes an entry in the registry to perform the  
//  translation of observations to state variables 
//  (for data assimilation)
// 
//  \begin{description}
//  \item[j]
//   name of the SubLSM + DA instance
//  \item[k]
//   index of the observation data
//  \end{description}
//EOP
{
  int len1;
  struct sublsmdaobstransformnode* current;
  struct sublsmdaobstransformnode* pnode;
  // create node

  len1 = len + 1; // ensure that there is space for terminating null
  pnode=(struct sublsmdaobstransformnode*) malloc(sizeof(struct sublsmdaobstransformnode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL;

  if(sublsmdaobstransform_table == NULL){
    sublsmdaobstransform_table = pnode;
  }
  else{
    current = sublsmdaobstransform_table;
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode;
  }

}

//BOP
// !ROUTINE: sublsmdaobstransform
// \label{sublsmdaobstransform}
//
// !INTERFACE:
void FTN(sublsmdaobstransform)(char *j, int *n, void *obs, int len)
//  
// !DESCRIPTION: 
//  Invokes the routine from the registry to translate
//  the observations to the prognostic variable space
//  (for data assimilation)
// 
//  \begin{description}
//  \item[j]
//   name of the SubLSM + DA instance
//  \item[n]
//   index of the nest
//   \item[obs]
//   transformed variable
//  \end{description}
//EOP
{
  struct sublsmdaobstransformnode* current;

  current = sublsmdaobstransform_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n");
      printf("obs transform routine for SubLSM + DA instance %s is not defined\n",j);
      printf("program will seg fault.....\n");
      printf("****************Error****************************\n");
    }
  }
  current->func(n,obs);
}

//BOP
// !ROUTINE: registersublsmdagetobspred
// \label{registersublsmdagetobspred}
// 
// !INTERFACE:
void FTN(registersublsmdagetobspred)(char *j, void (*func)(int*,int*,float*),int len)
//  
// !DESCRIPTION: 
//  Makes an entry in the registry for the routine 
//  that provides an LSM's estimate of the observations.
//  (for data assimilation)
// 
//  \begin{description}
//  \item[j]
//   name of the SubLSM + DA instance
//  \end{description}
//EOP
{
  int len1;
  struct sublsmdaobsprednode* current;
  struct sublsmdaobsprednode* pnode;
  // create node

  len1 = len + 1; // ensure that there is space for terminating null
  pnode=(struct sublsmdaobsprednode*) malloc(sizeof(struct sublsmdaobsprednode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL;

  if(sublsmdaobspred_table == NULL){
    sublsmdaobspred_table = pnode;
  }
  else{
    current = sublsmdaobspred_table;
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode;
  }

}

//BOP
// !ROUTINE: sublsmdagetobspred
// \label{sublsmdagetobspred}
//
// !INTERFACE:
void FTN(sublsmdagetobspred)(char *j, int *n, int *k,float *pred, int len)
//  
// !DESCRIPTION: 
//  Invokes the routine from the registry to translate
//  the observations to state variables
//  (for data assimilation)
// 
//  \begin{description}
//  \item[j]
//   name of the LSM
//  \item[n]
//   index of the nest
//  \item[pred]
//   model's estimated observation prediction
//  \end{description}
//EOP
{
  struct sublsmdaobsprednode* current;

  current = sublsmdaobspred_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;

    if(current==NULL) {
      printf("****************Error****************************\n");
      printf("obspred routine for SubLSM + DA instance %s is not defined\n",j);
      printf("program will seg fault.....\n");
      printf("****************Error****************************\n");
    }
  }
  current->func(n,k,pred);
}

//BOP
// !ROUTINE: registersublsmdadiagnosevars
// \label{registersublsmdadiagnosevars}
// 
// !INTERFACE:
void FTN(registersublsmdadiagnosevars)(char *j, void (*func)(int*),int len)
//  
// !DESCRIPTION:
//  Makes an entry in the registry for the routine to 
//  perform land surface model output
// 
//  \begin{description}
//  \item[j]
//   name of the LSM
//  \end{description}
//EOP
{
  int len1;
  struct sublsmdiagfordanode* current;
  struct sublsmdiagfordanode* pnode;
  // create node

  len1 = len + 1; // ensure that there is space for terminating null
  pnode=(struct sublsmdiagfordanode*) malloc(sizeof(struct sublsmdiagfordanode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL;

  if(sublsmdiagforda_table == NULL){
    sublsmdiagforda_table = pnode;
  }
  else{
    current = sublsmdiagforda_table;
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode;
  }

}
//BOP
// !ROUTINE: sublsmdadiagnosevars
// \label{sublsmdadiagnosevars}
//
// !INTERFACE:
void FTN(sublsmdadiagnosevars)(char *j, int *n, int len)
//  
// !DESCRIPTION:
//  Invokes the routine from the registry to perform
//  land surface model output
// 
//  \begin{description}
//  \item[j]
//   name of the LSM
//  \item[n]
//   index of the nest
//  \end{description}
//EOP
{

  struct sublsmdiagfordanode* current;

  current = sublsmdiagforda_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n");
      printf("diagnose for DA routine for SubLSM + DAset %s is not defined\n",j);
      printf("program will seg fault.....\n");
      printf("****************Error****************************\n");
    }
  }
  current->func(n);
}

//BOP
// !ROUTINE: registersublsmdamapobstosublsm
// \label{registersublsmdamapobstosublsm}
// 
// !INTERFACE:
void FTN(registersublsmdamapobstosublsm)(char *j, void (*func)(int*, int*, void*, void*),int len)
//  
// !DESCRIPTION: 
//  Makes an entry in the registry to perform the  
//  translation of observations to state variables 
//  (for data assimilation)
// 
//  \begin{description}
//  \item[j]
//   name of the LSM + DA instance
//  \end{description}
//EOP
{
  int len1;
  struct sublsmdamapobstosublsmnode* current;
  struct sublsmdamapobstosublsmnode* pnode;
  // create node

  len1 = len + 1; // ensure that there is space for terminating null
  pnode=(struct sublsmdamapobstosublsmnode*) malloc(sizeof(struct sublsmdamapobstosublsmnode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL;

  if(sublsmdamapobstosublsm_table == NULL){
    sublsmdamapobstosublsm_table = pnode;
  }
  else{
    current = sublsmdamapobstosublsm_table;
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode;
  }

}

//BOP
// !ROUTINE: sublsmdamapobstosublsm
// \label{sublsmdamapobstosublsm}
//
// !INTERFACE:
void FTN(sublsmdamapobstosublsm)(char *j, int *n, int *k, void *obs, void *sublsm, int len)
//  
// !DESCRIPTION: 
//  Invokes the routine from the registry to translate
//  the observations to state variables
//  (for data assimilation)
// 
//  \begin{description}
//  \item[j]
//   name of the LSM + DA instance
//  \item[n]
//   index of the nest
//  \item[obs]
//   observations to be mapped
//  \item[sublsm]
//   updated sublsm states  
//  \end{description}
//EOP
{
  struct sublsmdamapobstosublsmnode* current;

  current = sublsmdamapobstosublsm_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n");
      printf("map obs to LSM routine for LSM + DA instance %s is not defined\n",j);
      printf("program will seg fault.....\n");
      printf("****************Error****************************\n");
    }
  }
  current->func(n,k,obs,sublsm);
}
//BOP
// !ROUTINE: registersublsmdaqcstate
// \label{registersublsmdaqcstate}
// 
// !INTERFACE:
void FTN(registersublsmdaqcstate)(char *j, void (*func)(int*, void*),int len)
//  
// !DESCRIPTION: 
//  Makes an entry in the registry for the routine to 
//  QC the updated LSM state
//  (for data assimilation)
// 
//  \begin{description}
//  \item[j]
//   name of the LSM + DA instance
//  \end{description}
//EOP
{
  int len1;
  struct sublsmdaqcstatenode* current;
  struct sublsmdaqcstatenode* pnode;
  // create node

  len1 = len + 1; // ensure that there is space for terminating null
  pnode=(struct sublsmdaqcstatenode*) malloc(sizeof(struct sublsmdaqcstatenode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL;

  if(sublsmdaqcstate_table == NULL){
    sublsmdaqcstate_table = pnode;
  }
  else{
    current = sublsmdaqcstate_table;
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode;
  }

}
//BOP
// !ROUTINE: sublsmdaqcstate
// \label{sublsmdaqcstate}
// 
// !INTERFACE:
void FTN(sublsmdaqcstate)(char *j,int *n, void *LSM_State, int len)
//  
// !DESCRIPTION: 
//  Invokes the routine from the registry to set the 
//  QC the updated LSM variables
//  (for data assimilation)
// 
//  \begin{description}
//  \item[j]
//   name of the LSM
//  \item[n]
//   index of the nest
//  \item[LSM\_State]
//   The LSM state being qc'd
//  \end{description}
//EOP
{
  struct sublsmdaqcstatenode* current;

  current = sublsmdaqcstate_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n");
      printf("map obs to LSM routine for LSM + DA instance %s is not defined\n",j);
      printf("program will seg fault.....\n");
      printf("****************Error****************************\n");
    }
  }
  current->func(n,LSM_State);
}

//BOP
// !ROUTINE: registersublsmdaqcobsstate
// \label{registersublsmdaqcobsstate}
// 
// !INTERFACE:
void FTN(registersublsmdaqcobsstate)(char *j, void (*func)(int*, int*, void*),int len)
//  
// !DESCRIPTION: 
//  Makes an entry in the registry for the routine to 
//  QC the OBS state based on LSM variables and states
//  (for data assimilation).
// 
//  \begin{description}
//  \item[j]
//   name of the LSM + DA instance
//  \end{description}
//EOP
{
  int len1;
  struct sublsmdaqcobsnode* current;
  struct sublsmdaqcobsnode* pnode;
  // create node

  len1 = len + 1; // ensure that there is space for terminating null
  pnode=(struct sublsmdaqcobsnode*) malloc(sizeof(struct sublsmdaqcobsnode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL;

  if(sublsmdaqcobs_table == NULL){
    sublsmdaqcobs_table = pnode;
  }
  else{
    current = sublsmdaqcobs_table;
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode;
  }

}
//BOP
// !ROUTINE: sublsmdaqcobsstate
// \label{sublsmdaqcobsstate}
// 
// !INTERFACE:
void FTN(sublsmdaqcobsstate)(char *j, int *n, int *k, void *LSM_State, int len)
//  
// !DESCRIPTION: 
//  Invokes the routine from the registry to set the 
//  QC the observation state based on LSM variables and states
//  (for data assimilation).
// 
//  \begin{description}
//  \item[j]
//   name of the LSM
//  \item[n]
//   index of the nest
//  \item[LSM\_State]
//   The LSM state being qc'd
//  \end{description}
//EOP
{
  struct sublsmdaqcobsnode* current;

  current = sublsmdaqcobs_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n");
      printf("qc obs state routine for LSM + DA instance %s is not defined\n",j);
      printf("program will seg fault.....\n");
      printf("****************Error****************************\n");
    }
  }
  current->func(n,k,LSM_State);
}
//BOP
// !ROUTINE: registersublsmdascalestatevar
// \label{registersublsmdascalestatevar}
// 
// !INTERFACE:
void FTN(registersublsmdascalestatevar)(char *j, void (*func)(int*, void*),int len)
//  
// !DESCRIPTION: 
//  Makes an entry in the registry for the routine to 
//  scale the LSM state variables
//  (for data assimilation)
// 
//  \begin{description}
//  \item[j]
//   name of the LSM + DA instance
//  \item[j]
//   index of the assimilated variable
//  \end{description}
//EOP
{
  int len1;
  struct sublsmdascalenode* current;
  struct sublsmdascalenode* pnode;
  // create node

  len1 = len + 1; // ensure that there is space for terminating null
  pnode=(struct sublsmdascalenode*) malloc(sizeof(struct sublsmdascalenode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL;

  if(sublsmdascale_table == NULL){
    sublsmdascale_table = pnode;
  }
  else{
    current = sublsmdascale_table;
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode;
  }

}
//BOP
// !ROUTINE: sublsmdascalestatevar
// \label{sublsmdascalestatevar}
// 
// !INTERFACE:
void FTN(sublsmdascalestatevar)(char *j, int *n, void *LSM_State, int len)
//  
// !DESCRIPTION: 
//  Invokes the routine from the registry to scale the LSM 
//  state variables
//  (for data assimilation)
// 
//  \begin{description}
//  \item[j]
//   name of the LSM + DA instance
//  \item[n]
//   index of the nest
//  \item[LSM\_State]
//   The LSM state being scaled
//  \end{description}
//EOP
{
  struct sublsmdascalenode* current;

  current = sublsmdascale_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n");
      printf("scale variable routine for LSM + DA instance %s is not defined\n",j);
      printf("program will seg fault.....\n");
      printf("****************Error****************************\n");
    }
  }
  current->func(n,LSM_State);
}

//BOP
// !ROUTINE: registersublsmdadescalestatevar
// \label{registersublsmdadescalestatevar}
// 
// !INTERFACE:
void FTN(registersublsmdadescalestatevar)(char *j, void (*func)(int*, void*, void*),int len)
//  
// !DESCRIPTION: 
//  Makes an entry in the registry for the routine to 
//  descale the LSM state variables
//  (for data assimilation)
// 
//  \begin{description}
//  \item[j]
//   name of the LSM
//  \item[j]
//   index of the assimilated variable
//  \end{description}
//EOP
{
  int len1;
  struct sublsmdadescalenode* current;
  struct sublsmdadescalenode* pnode;
  // create node

  len1 = len + 1; // ensure that there is space for terminating null
  pnode=(struct sublsmdadescalenode*) malloc(sizeof(struct sublsmdadescalenode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL;

  if(sublsmdadescale_table == NULL){
    sublsmdadescale_table = pnode;
  }
  else{
    current = sublsmdadescale_table;
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode;
  }

}
//BOP
// !ROUTINE: sublsmdadescalestatevar
// \label{sublsmdadescalestatevar}
// 
// !INTERFACE:
void FTN(sublsmdadescalestatevar)(char *j,int *n, void *LSM_State, void *LSM_Incr_State, int len)
//  
// !DESCRIPTION: 
//  Invokes the routine from the registry descale the
//  LSM state variables 
//  (for data assimilation)
// 
//  \begin{description}
//  \item[j]
//   name of the LSM
//  \item[n]
//   index of the nest
//  \item[LSM\_State]
//   The LSM state being descaled
//  \end{description}
//EOP
{
  struct sublsmdadescalenode* current;

  current = sublsmdadescale_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n");
      printf("descale variables routine for SubLSM + DA instance %s is not defined\n",j);
      printf("program will seg fault.....\n");
      printf("****************Error****************************\n");
    }
  }
  current->func(n,LSM_State,LSM_Incr_State);
}
//BOP
// !ROUTINE: registersublsmdaupdatestate
// \label{registersublsmdaupdatestate}
// 
// !INTERFACE:
void FTN(registersublsmdaupdatestate)(char *j, void (*func)(int*, void*, void*),int len)
//  
// !DESCRIPTION: 
//  Makes an entry in the registry for the routine to 
//  apply the LSM state increments to LSM state
//  (for data assimilation)
// 
//  \begin{description}
//  \item[j]
//   name of the LSM + DA instance
//  \end{description}
//EOP
{
  int len1;
  struct sublsmdaupdatenode* current;
  struct sublsmdaupdatenode* pnode;
  // create node

  len1 = len + 1; // ensure that there is space for terminating null
  pnode=(struct sublsmdaupdatenode*) malloc(sizeof(struct sublsmdaupdatenode));
  pnode->name=(char*) calloc(len1,sizeof(char));
  strncpy(pnode->name,j,len);
  pnode->func = func;
  pnode->next = NULL;

  if(sublsmdaupdate_table == NULL){
    sublsmdaupdate_table = pnode;
  }
  else{
    current = sublsmdaupdate_table;
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode;
  }

}
//BOP
// !ROUTINE: sublsmdaupdatestate
// \label{sublsmdaupdatestate}
// 
// !INTERFACE:
void FTN(sublsmdaupdatestate)(char *j, int *n, void *LSM_State, void *LSM_Incr_State, int len)
//  
// !DESCRIPTION: 
//  Invokes the routine from the registry to apply the
//  LSM state increments to LSM State
//  (for data assimilation)
// 
//  \begin{description}
//  \item[j]
//   name of the LSM + DA instance
//  \item[n]
//   index of the nest
//  \item[LSM\_State]
//   The LSM state being updated
//  \item[LSM\_Incr\_State]
//   The LSM incr state
//  \end{description}
//EOP
{
  struct sublsmdaupdatenode* current;

  current = sublsmdaupdate_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n");
      printf("update state routine for LSM + DA instance %s is not defined\n",j);
      printf("program will seg fault.....\n");
      printf("****************Error****************************\n");
    }
  }
  current->func(n,LSM_State, LSM_Incr_State);
}




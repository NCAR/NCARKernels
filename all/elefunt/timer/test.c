

















































































 

















                                                                   































 


































                                                                   

















 




































































































































typedef unsigned long   size_t;




typedef long    fpos_t;




















































































typedef struct {
	int	_cnt;
	unsigned int	_flag2;
	unsigned char	*_ptr;
	unsigned char	*_base;
	int	_bufsiz;
	short	_flag;
	short	_file;


	void	*_unused;

	char	*__newbase;
	void	*_lock;			
	unsigned char	*_bufendp;
} FILE;

extern FILE	_iob[];









extern int     fread();
extern int     fwrite();



extern int	_flsbuf ();
extern int	_filbuf ();
extern int 	ferror ();
extern int 	feof ();
extern void 	clearerr ();
extern int 	putchar ();
extern int 	getchar ();
extern int 	putc ();
extern int 	getc ();
extern int	remove ();
extern int	rename ();
extern FILE 	*tmpfile ();
extern char 	*tmpnam ();
extern int 	fclose ();
extern int 	fflush ();
extern FILE	*fopen ();
extern FILE 	*freopen ();
extern void 	setbuf ();
extern int 	setvbuf ();
extern int	fprintf ();
extern int	fscanf ();
extern int	printf ();
extern int	scanf ();
extern int	sprintf ();
extern int	sscanf ();







































































typedef struct {
	char	**_a0;		
	int	_offset;		
} va_list;


extern int  vfprintf ();
extern int  vprintf ();
extern int  vsprintf ();



extern int 	fgetc ();
extern char 	*fgets ();
extern int 	fputc ();
extern int 	fputs ();
extern char 	*gets ();
extern int 	puts ();
extern int	ungetc ();
extern int	fgetpos ();
extern int 	fseek ();
extern int	fsetpos ();
extern long	ftell ();
extern void	rewind ();
extern void 	perror ();









































































 



























































typedef signed long     ptrdiff_t;










    typedef unsigned int  wchar_t;






typedef unsigned int wctype_t;






typedef int            time_t;




typedef int             clock_t;










typedef long                    ssize_t; 





typedef	unsigned char	uchar_t;
typedef	unsigned short	ushort_t;
typedef	unsigned int	uint_t;
typedef unsigned long	ulong_t;
typedef	volatile unsigned char	vuchar_t;
typedef	volatile unsigned short	vushort_t;
typedef	volatile unsigned int	vuint_t;
typedef volatile unsigned long	vulong_t;

















typedef int		level_t;
typedef	int		daddr_t;	
typedef	char *		caddr_t;	
typedef long *		qaddr_t;        
typedef char *          addr_t;
typedef	uint_t		ino_t;		
typedef short		cnt_t;
typedef int		dev_t;		
typedef	int		chan_t;		


typedef long    off_t;			


typedef unsigned long	rlim_t;		
typedef	int		paddr_t;
typedef	ushort_t	nlink_t;



typedef int    		key_t;		




typedef	uint_t		mode_t;		





typedef uint_t		uid_t;		





typedef uint_t		gid_t;		




typedef	void *		mid_t;		




typedef	int		pid_t;		


typedef char		slab_t[12];	

typedef ulong_t		shmatt_t;	
typedef ulong_t		msgqnum_t;	
typedef ulong_t		msglen_t;	



        typedef unsigned int wint_t;         





typedef unsigned long	sigset_t;











typedef struct __pthread_attr_t {
    long		__valid;
    char*		__name;
    unsigned long	__arg;
    unsigned long	__reserved[19];
    } pthread_attr_t;

typedef struct __pthread_mutexattr_t {
    long		__valid;
    unsigned long	__reserved[15];
    } pthread_mutexattr_t;

typedef struct __pthread_condattr_t {
    long		__valid;
    unsigned long	__reserved[13];
    } pthread_condattr_t;

typedef struct __pthread_t {
    void*		__reserved1;
    void*		__reserved2;
    unsigned short	__size;
    unsigned char	__reserved3[2];
    unsigned char	__reserved4[4];
    unsigned long	__sequence;
    unsigned long	__reserved5[2];
    void*		__per_kt_area;
    void*		__stack_base;
    void*		__stack_reserve;
    void*		__stack_yellow;
    void*		__stack_guard;
    unsigned long	__stack_size;
    void**		__tsd_values;
    unsigned long	__tsd_count;
    unsigned int	__reserved6;
    unsigned int	__reserved7;
    unsigned int	__thread_flags;
    } *pthread_t;

typedef volatile struct __pthread_mutex_t {
    unsigned int	__lock;
    unsigned int	__valid;
    char*		__name;
    unsigned int	__arg;
    unsigned int	__depth;
    unsigned long	__sequence;
    unsigned long	__owner;
    void*		__block;
    } pthread_mutex_t;

typedef volatile struct __pthread_cond_t {
    unsigned int	__state;
    unsigned int	__valid;
    char*		__name;
    unsigned int	__arg;
    unsigned long	__sequence;
    void*		__block;
    } pthread_cond_t;

typedef unsigned int	pthread_key_t;

typedef volatile struct __pthread_once_t {
    long	__state;
    long	__reserved[10];
    } pthread_once_t;









typedef long            timer_t;        

















typedef void (*sig_t) ();










typedef pid_t		id_t;		
					
					
					
					
typedef unsigned int	useconds_t;	







typedef uint_t	major_t;      
typedef uint_t	minor_t;      
typedef uint_t	devs_t;       
typedef uint_t	unit_t;       














































 











































typedef	unsigned long	vm_offset_t;
typedef	unsigned long	vm_size_t;







typedef	uchar_t		uchar;
typedef	ushort_t	ushort;
typedef	uint_t		uint;
typedef ulong_t		ulong;

typedef	physadr_t	physadr;



typedef	uchar_t		u_char;
typedef	ushort_t 	u_short;
typedef	uint_t		u_int;
typedef	ulong_t		u_long;
typedef	vuchar_t	vu_char;
typedef	vushort_t 	vu_short;
typedef	vuint_t		vu_int;
typedef	vulong_t	vu_long;



typedef struct  _quad { int val[2]; } quad;


typedef	long	swblk_t;
typedef u_long	fixpt_t;




























 




































typedef int	fd_mask;























typedef	struct fd_set {
	fd_mask	fds_bits[(((4096)+((  (sizeof(fd_mask) * 8		)	)-1))/(  (sizeof(fd_mask) * 8		)	))];
} fd_set;




















extern void bzero ();




struct timeval;
int select ();










typedef unsigned char sa_family_t;


typedef	unsigned int	in_addr_t;

typedef	unsigned short	in_port_t;











extern int 	fileno ();
extern FILE 	*fdopen ();


extern char *cuserid ();












































 










extern int getopt ();


extern char *optarg;
extern int optind;
extern int optopt;
extern int opterr;







extern char	*ctermid ();
extern int 	getw ();
extern int 	pclose ();
extern int 	putw ();
extern FILE 	*popen ();
extern char 	*tempnam ();


































 


































                                                                   











































 












































					



					


































































































 







































































					








































































 



















































































































































































extern void 	setbuffer ();
extern void 	setlinebuf ();




















































































extern  double acos();
extern  double asin();
extern  double atan();
extern  double atan2();
extern  double ceil();
extern  double cos();
extern  double cosh();
extern  double exp();
extern  double fabs();
extern  double floor();
extern  double fmod();
extern  double frexp();
extern  double ldexp();
extern  double log();
extern  double log10();
extern  double modf();
extern  double pow();
extern  double sin();
extern  double sinh();
extern  double sqrt();
extern  double tan();
extern  double tanh();



extern float acosf ();
extern float asinf ();
extern float atanf ();
extern float atan2f ();
extern float ceilf ();
extern float cosf ();
extern float coshf ();
extern float expf ();
extern float fabsf ();
extern float floorf ();
extern float fmodf ();
extern float frexpf ();
extern float ldexpf ();
extern float logf ();
extern float log10f ();
extern float modff ();
extern float powf ();
extern float sinf ();
extern float sinhf ();
extern float sqrtf ();
extern float tanf ();
extern float tanhf ();


















extern int signgam;


extern double   erf();
extern double   erfc();
extern double   gamma();
extern double   hypot();
extern int      isnan();
extern double   j0();
extern double   j1();
extern double   jn();
extern double   lgamma();
extern double   y0();
extern double   y1();
extern double   yn();





extern float erfcf ();
extern float erff ();
extern float gammaf ();
extern float hypotf ();
extern int   isnanf ();
extern float j0f ();
extern float j1f ();
extern float jnf ();
extern float lgammaf ();
extern float y0f();
extern float y1f();
extern float ynf();





































 


















                                                                   































typedef struct div_t  {			
	int quot;			
	int rem; } div_t;			

typedef struct ldiv_t  {		
	long int quot;			
	long int rem; } ldiv_t;		












extern int __getmbcurmax ();




extern int	mblen ();
extern size_t	mbstowcs ();
extern int	mbtowc ();
extern size_t	wcstombs ();
extern int	wctomb ();


extern int	rpmatch ();

extern void 	*valloc ();


extern double 	atof ();
extern int 	atoi ();
extern long int atol ();
extern double 	strtod ();
extern long int strtol ();
extern unsigned long int strtoul ();
extern int 	rand ();
extern void	srand ();
extern void 	*calloc ();
extern void	free ();
extern void	*malloc ();
extern void 	*realloc ();






extern long	a64l ();
extern char	* l64a ();
extern int	ttyslot ();


extern char	*ptsname ();








extern void	abort ();
extern int	atexit ();
extern void	exit ();
extern char	*getenv ();
extern int 	system ();
extern void 	*bsearch ();
extern void 	qsort ();
extern int 	abs ();
extern struct div_t	div ();
extern long int	labs ();
extern struct ldiv_t 	ldiv ();










extern int	_Prand_r ();













extern double	drand48 ();
extern double	erand48 ();
extern long	jrand48 ();
extern void	lcong48 ();
extern long	lrand48 ();
extern long	mrand48 ();
extern long	nrand48 ();
extern unsigned short *seed48 ();
extern void	srand48 ();
extern int 	putenv ();
extern void	setkey ();













extern char	*initstate ();
extern char	*setstate ();
extern int	grantpt ();
extern int	getsubopt ();

extern int	random ();
extern int	srandom ();

extern char 	*realpath ();
extern int	unlockpt ();











extern int 	clearenv ();
extern char 	*getpass ();




extern char 	*mktemp ();
extern int 	mkstemp ();












char	*ecvt ();
char	*fcvt ();
char	*gcvt ();















 

extern double   acosh();
extern double   asinh();
extern double   atanh();
extern double   cbrt();
extern double   expm1();
extern double   log1p();
extern double   logb();
extern double   nextafter();
extern double   remainder();
extern double   rint();



extern double   cabs();
extern double   copysign ();
extern double   drem();


extern double   scalb();

extern int      finite();




extern float acoshf ();
extern float asinhf ();
extern float atanhf ();
extern float cabsf ();
extern float cbrtf ();
extern float copysignf ();
extern float dremf ();
extern float expm1f ();
extern float log1pf ();
extern float logbf ();
extern float nextafterf ();
extern float remainderf ();
extern float rintf ();
extern float scalbf ();
extern int finitef ();




extern double F_acos ();
extern double F_asin ();
extern double F_atan ();
extern double F_atan2 ();
extern double F_cos ();
extern double F_exp ();
extern double F_hypot ();
extern double F_log ();
extern double F_log10 ();
extern double F_pow ();
extern double F_sin ();
extern double F_sqrt ();
extern double F_tan ();




extern float F_acosf ();
extern float F_asinf ();
extern float F_atan2f ();
extern float F_atanf ();
extern float F_cosf ();
extern float F_expf ();
extern float F_hypotf ();
extern float F_log10f ();
extern float F_logf ();
extern float F_powf ();
extern float F_sinf ();
extern float F_sqrtf ();
extern float F_tanf ();




extern double acosd ();
extern double asind ();
extern double atand ();
extern double atand2 ();
extern double cosd ();
extern double cot ();
extern double cotd ();
extern double log2 ();
extern double nint ();
extern double powi ();
extern double sind ();
extern double tand ();
extern double trunc ();

extern float acosdf ();
extern float asindf ();
extern float atand2f ();
extern float atandf ();
extern float cosdf ();
extern float cotdf ();
extern float cotf ();
extern float log2f ();
extern float nintf ();
extern float powif ();
extern float sindf ();
extern float tandf ();
extern float truncf ();

extern int fp_class ();
extern int fp_classf ();
extern int powii ();
extern int unordered ();
extern int unorderedf ();
























struct exception {
	int type;
	char *name;
	double arg1;
	double arg2;
	double retval;
};















































































































































































 






































struct timeval {
	time_t	tv_sec;		
	int	tv_usec;	
		
		
};

struct	itimerval {
	struct		timeval it_interval; 
	struct		timeval it_value; 
};






























































































































































































































































































































































































































































                                                                   









































































































































































































































































































































struct sigaction {

	union {
		 
		void	(*_handler) ();
		void	(*_sigaction) ();
	} _sa_un;








	sigset_t sa_mask;	
	int	sa_flags;	
	int	sa_signo;	
};







extern int kill (); 




extern int sigaction
	(); 



extern int sigprocmask ();
extern int sigsuspend ();  

extern int sigemptyset ();
extern int sigfillset ();
extern int sigaddset ();
extern int sigdelset ();
extern int sigismember ();
extern int sigpending ();



extern int sigwaitinfo ();
extern int sigtimedwait (); 
extern int sigqueue ();

















































 

















                                                                   









































































struct  sigcontext {


	



	long    sc_onstack;		
	long    sc_mask;		
	long	sc_pc;			
	long	sc_ps;			
	long	sc_regs[32];		
	long	sc_ownedfp;		
	long	sc_fpregs[32];		
	unsigned long sc_fpcr;		
	unsigned long sc_fp_control;	
	


	long sc_reserved1;		
	int sc_kreserved1;		
	int sc_kreserved2;		



	size_t	sc_ssize;		
	caddr_t	sc_sbase;		
	unsigned long sc_traparg_a0;	
	unsigned long sc_traparg_a1;	
	unsigned long sc_traparg_a2;	
	unsigned long sc_fp_trap_pc;	
	unsigned long sc_fp_trigger_sum; 
	unsigned long sc_fp_trigger_inst; 
};


























































extern sigset_t cantmasksigset;
























					
































       

















































extern int sigpause ();




extern int sigreturn ();
extern int sigblock ();
extern int sigsetmask ();


extern int sigstack ();
extern int siginterrupt ();






extern void (*sigset ()) ();



extern int sighold ();
extern int sigrelse ();



extern int sigignore ();




extern int sigaltstack ();








































typedef enum idop {

	POP_DIFF,	



	POP_AND,	



	POP_OR,	




	POP_XOR	





} idop_t;






typedef enum idtype {

	P_PID,	
	P_PPID,	
	P_PGID,	
	P_SID,	
	P_CID,	
	P_UID,	
	P_GID,	
	P_ALL	
} idtype_t;







typedef struct procset {
	idop_t p_op;		




	idtype_t p_lidtype;	
	id_t p_lid;		

	idtype_t p_ridtype;	
	id_t p_rid;		
} procset_t;

















extern int sigsendset ();
extern int sigsend ();

int (*ssignal ()) ();
int gsignal ();













































 






















                                                                   































struct	tm {			
        int     tm_sec;         
        int     tm_min;         
        int     tm_hour;        
        int     tm_mday;        
        int     tm_mon;         
        int     tm_year;        
        int     tm_wday;        
        int     tm_yday;        
        int     tm_isdst;       

        long    tm_gmtoff;
        char    *tm_zone;

};






extern clock_t 	clock ();
extern double 	difftime ();
extern time_t 	mktime ();
extern time_t 	time ();
extern char 	*asctime ();
extern char 	*ctime ();
extern struct tm *gmtime ();
extern struct tm *localtime ();
extern size_t 	strftime ();












extern char * _Pasctime_r ();
extern char * _Pctime_r ();
extern struct tm * _Pgmtime_r ();
extern struct tm * _Plocaltime_r ();







 

















































 
























typedef int             clockid_t;










struct	itimerspec {
	struct		timespec it_interval; 
	struct		timespec it_value; 
};






















int clock_gettime ();
int clock_settime ();
int clock_getdrift ();
int clock_setdrift ();
int timer_create ();
int timer_delete ();
int timer_gettime ();
int timer_settime ();
int timer_getoverrun ();
int nanosleep ();
int clock_getres ();













extern char *tzname[];

extern void tzset ();




extern long timezone;


extern int daylight;

extern char *strptime ();

















extern unsigned char *NLctime ();
extern unsigned char *NLasctime ();
extern char *NLstrtime ();

extern struct tm *getdate ();
extern int getdate_err;

























 

























struct timezone {
	int	tz_minuteswest;	
	int	tz_dsttime;	
};













extern int adjtime ();
extern int settimeofday ();



extern int setitimer ();
extern int gettimeofday ();
extern int utimes ();

extern int getitimer ();







































 




































































struct	rusage {
	struct timeval ru_utime;	
	struct timeval ru_stime;	
	long	ru_maxrss;

	long	ru_ixrss;		
	long	ru_idrss;		
	long	ru_isrss;		
	long	ru_minflt;		
	long	ru_majflt;		
	long	ru_nswap;		
	long	ru_inblock;		
	long	ru_oublock;		
	long	ru_msgsnd;		
	long	ru_msgrcv;		
	long	ru_nsignals;		
	long	ru_nvcsw;		
	long	ru_nivcsw;		

};























struct rlimit {
	rlim_t	rlim_cur;		
	rlim_t	rlim_max;		
};







struct rusage_dev {
	struct rusage ru_rusage;
	dev_t	      ru_dev;
};
















extern int getpriority ();
extern int setpriority ();
extern int setrlimit ();

extern int getrlimit ();
extern int getrusage ();





























struct profil_args {
	unsigned int 	*buffer; 
	void		*highpc; 
	void		*lowpc;  
	unsigned long 	scale; 	 
};




















struct rusage
   rusage1, rusage2;

double timer_clock()
{






   getrusage(0, &rusage2);

   return( (double) rusage2.ru_utime.tv_sec +
          ((double) rusage2.ru_utime.tv_usec) * 1.e-6 +
           (double) rusage2.ru_stime.tv_sec +
          ((double) rusage2.ru_stime.tv_usec) * 1.e-6);
   }








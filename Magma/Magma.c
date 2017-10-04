function ITB(a)
//input: a->integer
//output: sequence of 32 bits(from most important to less)
s:=Reverse(Intseq(a,2));
for i in [1..(32-#s)] do
s:=Insert(s, 1, 0);
end for;
return s;
end function;

function ITB64(a)
//input: a->integer
//output: sequence of 64 bits(from most important to less)
s:=Reverse(Intseq(a,2));
for i in [1..(64-#s)] do
s:=Insert(s, 1, 0);
end for;
return s;
end function;


function ITB4(a)
//input: a->integer
//output: sequence of 4 bits(from most important to less)
s:=Reverse(Intseq(a,2));
for i in [1..(4-#s)] do
s:=Insert(s, 1, 0);
end for;
return s;
end function;


function BTI(a)
//input: a->sequence of bits(from most important to less)
//output: integer
return Seqint(Reverse(a), 2);
end function;


function STB(a)
//input: a->string of hexadecimal element
//output: sequence of bit
b:=Eltseq(a);
c:=[];
for i in [1..#b] do
c:=c cat ITB4(StringToInteger(b[i],16));
end for;
return c;
end function;

function BTS(h)
//input: h-> sequence of bit
//output: string of hexadecimal element
c:="";
for i in [1..7] do
b:=Partition(h[i], 4);
for j in [1..#b] do
c:=c cat IntegerToString(BTI(b[j]),16);
end for;
end for;
return c;
end function;


function SUM(a,b)
//input:a,b-> sequences of 32 bits
//output: sequence of addition of a, b mod 2^32
c:=BTI(a);
d:=BTI(b);
return ITB((c+d) mod 2^32);
end function;

function XOR(a,b)
//a,b->sequences of 32 binary elements
C:=[];
for i in [1..32] do
C[i]:=(a[i]+b[i]) mod 2;
end for;
return C;
end function;

function Initial_value()
IV:=[];
IV[1]:=0xC1059ED8;
IV[2]:=0x367CD507;
IV[3]:=0x3070DD17;
IV[4]:=0xF70E5939;
IV[5]:=0xFFC00B31;
IV[6]:=0x68581511;
IV[7]:=0x64F98FA7;
IV[8]:=0xBEFA4FA4;
c_h:=[];
c_h[1]:=0x243F6A88;
c_h[2]:=0x85A308D3;
c_h[3]:=0x13198A2E;
c_h[4]:=0x03707344;
c_h[5]:=0xA4093822;
c_h[6]:=0x299F31D0;
c_h[7]:=0x082EFA98;
c_h[8]:=0xEC4E6C89;
c_h[9]:=0x452821E6;
c_h[10]:=0x38D01377;
c_h[11]:=0xBE5466CF;
c_h[12]:=0x34E90C6C;
c_h[13]:=0xC0AC29B7;
c_h[14]:=0xC97C50DD;
c_h[15]:=0x3F84D5B5;
c_h[16]:=0xB5470917;
h:=[];
c:=[];
for i in [1..8] do
h[i]:=ITB(IV[i]);
c[i]:=ITB(c_h[i]);
end for;
for i in [9..16] do
c[i]:=ITB(c_h[i]);
end for;
return h, c;
end function;


function initialization(h, const, t)
//h sequence of 8 sequences of 32 bits
//const->sequence of 16 sequences of 32 bits
//t sequence of 2 sequences of 32 bits
v:=[];
for i in [1..8] do
v[i]:=h[i];
end for;
for i in [1..4] do
v[i+8]:=const[i];
end for;
v[13]:=XOR(t[1], const[5]);
v[14]:=XOR(t[1], const[6]);
v[15]:=XOR(t[2], const[7]);
v[16]:=XOR(t[2], const[8]);
return v;
end function;

function sigma(r,a)
//r the round from 1 to 10
//a argument from 0 to 15
s:=[[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15],
  [14,10,4,8,9,15,13,6,1,12,0,2,11,7,5,3],
  [11,8,12,0,5,2,15,13,10,14,3,6,7,1,9,4],
  [7,9,3,1,13,12,11,14,2,6,5,10,4,0,15,8],
  [9,0,5,7,2,4,10,15,14,1,11,12,6,8,3,13],
  [2,12,6,10,0,11,8,3,4,13,7,5,15,14,1,9],
  [12,5,1,15,14,13,4,10,0,7,6,3,9,2,8,11],
  [13,11,7,14,12,1,3,9,5,0,15,4,8,6,2,10],
  [6,15,14,9,11,3,0,8,12,2,13,7,1,4,10,5],
  [10,2,8,4,7,6,1,5,15,11,9,14,3,12,13,0]];
return s[r][a+1];
end function;


function G(i,a,b,c,d,m,const,r)
//a,b,c,d->sequences of 32 binary elements
//const->sequence of 16 sequences of 32 bits
//m->sequence of 16 sequences of 32 bits
//r->the round from 1 to 14
a:=SUM(SUM(a,b),XOR(m[sigma(r,2*i)+1],const[sigma(r,2*i+1)+1]));
d:=Rotate(XOR(d,a),16);
c:=SUM(c,d);
b:=Rotate(XOR(b,c),12);
a:=SUM(SUM(a,b),XOR(m[sigma(r,2*i+1)+1],const[sigma(r,2*i)+1]));
d:=Rotate(XOR(d,a),8);
c:=SUM(c,d);
b:=Rotate(XOR(b,c),7);
return a,b,c,d;
end function;

function round(v, m, c)
for i in [1..10] do
v[1],v[5],v[9],v[13]:=G(0,v[1],v[5],v[9],v[13],m,c,i);
v[2],v[6],v[10],v[14]:=G(1,v[2],v[6],v[10],v[14],m,c,i);
v[3],v[7],v[11],v[15]:=G(2,v[3],v[7],v[11],v[15],m,c,i);
v[4],v[8],v[12],v[16]:=G(3,v[4],v[8],v[12],v[16],m,c,i);
v[1],v[6],v[11],v[16]:=G(4,v[1],v[6],v[11],v[16],m,c,i);
v[2],v[7],v[12],v[13]:=G(5,v[2],v[7],v[12],v[13],m,c,i);
v[3],v[8],v[9],v[14]:=G(6,v[3],v[8],v[9],v[14],m,c,i);
v[4],v[5],v[10],v[15]:=G(7,v[4],v[5],v[10],v[15],m,c,i);
end for;
return v;
end function;



function finalization(v,h)
for i in [1..8] do
h[i]:=XOR(h[i],XOR(v[i],v[i+8]));
end for;
return h;
end function;



function compress(h,m,c,t)
v:=initialization(h, c, t);
v:=round(v,m,c);
return(finalization(v,h));
end function;


function padding(m, l);
//m string
//l lenght
a:=STB(m);
a:=a[[1..l]];   //#a = l
a:=Append(a,1); //#a = l+1
q:=l div 512;
r:=l mod 512;
j:=[];
if  (r le 446) and (r ne 0) then
for i in [1..q] do
j[i]:=512*i;
end for;
j[q+1]:=l;
for i in [(#a+1) mod 512..447] do
a:=Append(a,0);
end for;
end if;
if r eq 0 then
for i in [1..q] do
j[i]:=512*i;
end for;
j[q+1]:=0;
for i in [(#a+1) mod 512..447] do
a:=Append(a,0);
end for;
end if;
if r ge 447 then
for i in [1..q] do
j[i]:=512*i;
end for;
j[q+1]:=l;
j[q+2]:=0;
for i in [(#a+1)..447+(q+1)*512] do
a:=Append(a,0);
end for;
end if;
//#a mod 512 = 447
a:=Append(a,0);
b:=ITB64(l);
a:=a cat b;
me:=Partition(a, 512);
mes:=[];
for i in [1..#me] do
mes[i]:=Partition(me[i],32);
end for;
len:=[];
for i in [1..#j] do
len[i]:=Rotate(Partition(ITB64(j[i]),32),1);
end for;
return len, mes;
end function;


function blake224(mes, len)
h,c:=Initial_value();
l, m:=padding(mes,len);
for i in [1..#m] do
h:=compress(h,m[i],c,l[i]);
end for;
return BTS(h);
end function;

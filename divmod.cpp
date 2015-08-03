#include <bits/stdc++.h>
using namespace std;


typedef long long ll;
const ll base=100000000;
const ll pow_base=8;

#define CMP(a,b) ((a)>(b)?1:(a)==(b)?0:-1)
struct bigint{
	int sign;
	vector<ll> digits;
	
	bigint(){
		sign=1;
		digits.clear();
	}
	bigint(char *s){
		//cout<<s<<endl;
		int ind=0,len=strlen(s);
		int cnt=0;
		ll value=0;
		ll pw=1;
		if(s[0]=='-'){
			sign=-1;
			ind++;
		}else{
			sign=1;
		}
		
		while(--len>=ind){
			cnt++;
			value+=pw*(s[len]-'0');
			//cout<<value<<endl;
			if(len==ind or cnt==pow_base){
				digits.push_back(value);
				cnt=0;
				pw=1;
				value=0;
			}else{
				pw*=10;
			}
		}
		//cout<<*this<<endl;
	}
	bigint(ll x){
		if(x<0){
			sign=-1;
			x=-x;
		}else{
			sign=1;
		}
		while(x){
			digits.push_back(x%base);
			x/=base;
		}
	}
	friend ostream & operator<<(ostream &out,const bigint &a){
		if(!a.digits.size()){
			out<<"0";
			return out;
		}
		if(a.sign<0){
			out<<"-";
		}	
		out<<a.digits.back();
		int i;
		for(i=a.digits.size()-2;i>=0;--i){
			out<<setw(pow_base)<<setfill('0')<<a.digits[i];
		}
		return out;
		
	}
	
	void mul_to(ll x){
		if(x<0){
			sign=-sign;
			x=-x;
		}
		int i;
		ll rem=0;
		for(int i=0;i<digits.size();++i){
			rem+=digits[i]*x;
			digits[i]=rem%base;
			rem/=base;
		}
		while(rem){
			digits.push_back(rem%base);
			rem/=base;
		}
		trim();
	}
	void trim(){
		while(digits.size() and digits.back()==0) digits.pop_back();
	}
	ll div_to(ll x){
		assert(x);
		ll rem=0;
		if(x<0){
			sign=-sign;
			x=-x;
		}
		int i;
		for(i=digits.size()-1;i>=0;--i){
			rem=digits[i]+rem*base;
			digits[i]=rem/x;
			rem%=x;
		}
		trim();
		return rem;
	}
	void _add_to(bigint & b){ // add abs
		int i;
		ll rem=0;
		digits.resize(max(digits.size(),b.digits.size()),0);
		for(i=0;i<digits.size();i++){
			rem+=digits[i]+(i<b.digits.size()?b.digits[i]:0);
			digits[i]=rem%base;
			rem/=base;
		}
		while(rem){
			digits.push_back(rem%base);
			rem/=base;
		}
	}
	void _sub_to(bigint &b){  //abs(*this) >= b
		ll memo=0;
		int i;
		for(i=0;i<digits.size();i++){
			ll t=digits[i]-(i<b.digits.size()?b.digits[i]:0)+memo;
			if(t<0){
				digits[i]=t+base;
				memo=-1;
			}else{
				digits[i]=t;
				memo=0;
			}
		}
		trim();
	}
	int cmp(bigint &b){
		if(digits.size()!=b.digits.size()){
			return CMP(digits.size(),b.digits.size());
		}
		int i;
		for(i=digits.size()-1;i>=0;--i){
			if(digits[i]!=b.digits[i]){
				return CMP(digits[i],b.digits[i]);
			}
		}
		return 0;
	}
	bigint operator+(bigint &b){
		bigint res;
		if(sign==b.sign){
			res=*this;
			res._add_to(b);
		}else{
			if(this->cmp(b)>=0){
				res=*this;
				res._sub_to(b);
				res.sign=sign;
			}else{
				res=b;
				res._sub_to(*this);
				res.sign=b.sign;
			}
		}
		return res;
	}
	bigint operator-(bigint &b){
		bigint tmp=b;
		tmp.sign=-tmp.sign;
		return (*this)+tmp;
	
	}
	void shift(int k){
		int i;
		digits.resize(digits.size()+k);
		for( i=digits.size()-1;i>=k;--i){
			digits[i]=digits[i-k];
		}
		for( i=0;i<k;++i){
			digits[i]=0;
		}
	}
	bigint operator*(bigint &b){
		bigint res;
		int i;
	
		for(i=b.digits.size()-1;i>=0;--i){
			bigint temp=*this;
			res.shift(1);
			temp.mul_to(b.digits[i]);
			res=res+temp;
		}
		res.sign=sign*b.sign;
		res.trim();
		return res;
	}
	static pair<bigint,bigint> divmod(bigint &_a,bigint &_b ){
		bigint q,r;
		bigint a=_a,b=_b;
		if(b.digits.size()==1){
			ll rem=a.div_to(b.digits.back());
			q=a;
			r=bigint(rem);
		}else{
			int i,k=b.digits.size()/2;
			bigint t,a1,b1,a0,b0,q1,r1,ta,tb;
			
			b1.sign=b.sign;
			for(i=k;i<b.digits.size();++i){
				b1.digits.push_back(b.digits[i]);
			}
			
			b0.sign=b.sign;
			for(i=0;i<k;++i){
				b0.digits.push_back(b.digits[i]);
			}
			
			ta.sign=a.sign;
			for(i=2*k;i<a.digits.size();++i){
				ta.digits.push_back(a.digits[i]);
			}
			
			a0.sign=a.sign;
			for(i=0;i<min(2*k,(int)a.digits.size());++i){
				a0.digits.push_back(a.digits[i]);
			}
			
			pair<bigint,bigint> tt=divmod(ta,b1);
			q1=tt.first;
			r1=tt.second;
			
			r1.shift(2*k);
			ta=r1+a0;
			t=q1*b0;
			t.shift(k);
			
			tb=ta-t;
			ta=tb;
			tb=b;
			tb.shift(k);
			
			bigint ONE(1);
			while(ta.sign<0){
				q1=q1-ONE;
				ta=ta+tb;
			}
			
			bigint tta;
			tta.sign=ta.sign;
			for(i=k;i<ta.digits.size();++i){
				tta.digits.push_back(ta.digits[i]);
			}
			
			bigint q0,r0;
			
			a0.digits.clear();
			a0.sign=ta.sign;
			for(i=0;i<min(k,(int)ta.digits.size());++i){
				a0.digits.push_back(ta.digits[i]);
			}
			
			tt=divmod(tta,b1);
			q0=tt.first;
			r0=tt.second;
			
			r0.shift(k);
			
			t=a0+r0;
			
			tta=t;
			t=q0*b0;
			a0=tta-t;
			tta=a0;
			
			while(tta.sign<0){
				q0=q0-ONE;
				tta=tta+b;
			}
			q1.shift(k);
			q=q1+q0;
			r=tta;
		}
		return pair<bigint,bigint>(q,r);
	}
};


int main() {
	char temp[100];
	strcpy(temp,"12312312312135123121");
	bigint a(temp);
	
	strcpy(temp,"12312321312312");
	bigint b(temp);
	cout<<a<<endl;
	cout<<b<<endl;
	pair<bigint,bigint> qr=bigint::divmod(a,b);
	cout<<qr.first<<" "<<qr.second<<endl;
	return 0;
}
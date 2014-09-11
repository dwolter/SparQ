/* This file is part of SparQ, a toolbox for qualitative spatial reasoning.
 Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Project R3-[Q-Shape]
 More info at http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/
 
 SparQ is free software and has been released under the terms of the GNU
 General Public License version 3 or later. You should have received a
 copy of the GNU General Public License along with this program. If not,
 see <http://www.gnu.org/licenses/>.
 */


#include "Interface.h"
#include "QTC.h"
#include "qualifier.h"
#include <string>
#include <iostream>

using namespace std;

/* Output buffer */
char cres[MAX_STRBUF];
string aB21relations0[] = { "--", "-0", "-+", "0-", "00", "0+", "+-", "+0", "++", };
string aB21relationsO[] = { "--", "-O", "-+", "O-", "OO", "O+", "+-", "+O", "++", };
// List of invald 5bit relations regarding WTC-C22* (i.e. no angle constraint)
string aDelList0[] ={ "-0-00", "-0-0-", "-0000", "-000-", "-0+00", "-0+0-",
    "0-0-+", "0-0-0", "0-00+", "0-000", "0-0++", "0-0+0",
    "00-00", "00-0-", "000-+", "000-0", "0000+",
    "0000-", "000++", 
    "000+0", "00+00", "00+0-", "0+0-+", "0+0-0",
    "0+00+", "0+000", "0+0++", "0+0+0", "+0-00", "+0-0-",
    "+0000", "+000-", "+0+00", "+0+0-"}; //
string aDelListO[] ={ "-O-OO", "-O-O-", "-OOOO", "-OOO-", "-O+OO", "-O+O-",
    "O-O-+", "O-O-O", "O-OO+", "O-OOO", "O-O++", "O-O+O",
    "OO-OO", "OO-O-", "OOO-+", "OOO-O", "OOOO+",
    "OOOO-", "OOO++", 
    "OOO+O", "OO+OO", "OO+O-", "O+O-+", "O+O-O",
    "O+OO+", "O+OOO", "O+O++", "O+O+O", "+O-OO", "+O-O-",
    "+OOOO", "+OOO-", "+O+OO", "+O+O-"}; //
char symbol[] = "-0+";


/* cleans up result cres for qtc-c22* by deleting any invalid relation; cf. aDelList */
int cleanCres_qtc_c22( )
{
    // printf("Clean result by means of dellist ... TODO\n");
    string result = cres;
    string new_result = "(";
    if (result.compare("()")==0) {
        return true;
    }
    //cout << "BEFORE: " << result << " (size: " << result.size() << ")" << endl;
    int i_pos = 1;
    int len=sizeof(aDelListO)/sizeof(string);
    while (result[i_pos]!=')') {
        //cout << "\t subrel(" << i_pos << "): " << result.substr(i_pos,5) << endl; 
        string subrel=result.substr(i_pos,5);
        bool contained=false;
        for (int i=0; i<len; i++) {
            if (aDelListO[i]==subrel) {
                // cout << "\tdelete invalid rel " << subrel << endl; 
                contained=true;
                i=len;
            }
        }
        if (!contained) {
            new_result.append(subrel);
            new_result.append(" ");
        }

        i_pos = i_pos+5;
        while (result[i_pos]==' ') {
            i_pos++;
        }
    }
    new_result.append(")");
 
    //cout << "size new result = " << new_result.length() << ": " << new_result << endl;
    for (int i=0; i<new_result.length(); i++) {
        cres[i]=new_result[i];
    }
    // printf( "AFTER : %s \n", cres);
    return 1;
}

/* Returns a std::string by copying it to the output buffer and returning a pointer to the buffer */
const char* return_result(std::string the_result)
{
	if (the_result.length() < MAX_STRBUF)
	{
		sprintf(cres,"%s",the_result.c_str());
	} else {
		sprintf(cres,"OBACHT!-Stringbuffer-zu-klein!");
	}
    return cres;
}
/* Parses a string (e.g. "++++++") and returns a relation struct {+,+,+,+,+,+} */
void parseRelation(const char* relation_name, struct relation* rel)
{
    string relName(relation_name);
    rel->length = relName.length();
    for(int i=0; i<rel->length; i++){
        if( relation_name[i]!='-' || relation_name[i]!='0' || relation_name[i]!='+' ){
            printf("Invalid Relation part %c of %s subsituted with 0 ... \n", relName.at(i), relName.c_str());
            rel->c[i]=0;
        }
        else{
            rel->c[i]=relation_name[i];
        }
    }
}


// Velocity Qualify
string qtc_general_velocity_qualify( float vK, float vL){
    if( vK>vL ) return string( "+" );
    if( vK<vL ) return string( "-" );
    return string( "0" );
}
// Velocity Composition
string qtc_general_velocity_composition( string sK, string sL){
    string result = "X";
    string input = sK + sL;
    
    if ( input == "--" ) result = "-";
    if ( input == "-0" ) result = "-";
    if ( input == "-O" ) result = "-";
    if ( input == "-+" ) result = "A";
    if ( input == "0-" ) result = "-";
    if ( input == "O-" ) result = "-";
    if ( input == "00" ) result = "0";
    if ( input == "OO" ) result = "0";
    if ( input == "0+" ) result = "+";
    if ( input == "O+" ) result = "+";
    if ( input == "+-" ) result = "A";
    if ( input == "+0" ) result = "+";
    if ( input == "+O" ) result = "+";
    if ( input == "++" ) result = "+";
    // printf(" %s => %s\n", input.c_str(), result.c_str());
    
    return result;
}
// Velocity Converse
char qtc_general_velocity_converse( char vel ){
    if(      vel=='-' ) return '+' ;
    else if( vel=='+' ) return '-' ;
    else                return '0' ;
}



// *** Angle ***
// Currently no composition of angle labels known (compare PhD of N.v.d.Weghe)
// probably not possible without "DISTANCE" => dummy function
// ANGLE IS CURRENTLY NOT PART OF C22 RELATIONS, i.e. only 5 characters
// Angle Qualify
string qtc_general_angle_qualify( float vK, float vL){
    if( vK>vL ) return string( "+" );
    if( vK<vL ) return string( "-" );
    return string( "0" );
}
// Angle Composition
string qtc_general_angle_composition( string vK, string vL){
    return string("0");
}
// Angle Converse
string qtc_general_angle_converse( string vel){
    if( vel=="-" ) return string( "+" );
    if( vel=="+" ) return string( "-" );
    return string( "0" );
}



/******************************************************************************
 ******************************************************************************
 *** QTC-C22
 ******************************************************************************
 *****************************************************************************/
/******************************************************************************
 *** COMPOSITION
 *****************************************************************************/

const char* qtc_c22_composition(const char* foo, const char* rel_ab_name , const char* rel_bc_name){
    string sRel_AB( rel_ab_name );
    string sRel_BC( rel_bc_name );
    //printf( "INPUT: |%s| AND |%s|\n", rel_ab_name, rel_bc_name);
    
    //  sprintf( sAB, "%s", rel_ab_name);
    //  sprintf( sBC, "%s", rel_bc_name);
    for(int i=0; i<sRel_AB.length(); i++){
        if( sRel_AB.at(i)=='O' || sRel_AB.at(i)=='o'){ sRel_AB[i]='0';}    
        if( sRel_BC.at(i)=='O' || sRel_BC.at(i)=='o'){ sRel_BC.at(i)='0';}
    }
    //printf( "INPUT MODIFIED: |%s| AND |%s|\n", sRel_AB.c_str(), sRel_BC.c_str());
    
    string v1 = sRel_AB.substr(4,1);
    string v2 = sRel_BC.substr(4,1);
    string rel_ab( sRel_AB );
    string rel_bc( sRel_BC );
    string v = qtc_general_velocity_composition( v1, v2);
    if( !(v=="-" || v=="+" || v!="0" || v!="O" || v!="o" || v!="A")) {
        printf( "ERROR in velocity part of composition (%s)... EXIT!!!!\n", v.c_str() );
        exit(EXIT_FAILURE);
    }
    string base = getQTCcomposition( sRel_AB, sRel_BC );
    int iCres=0;
    
    //printf( "********************************************** 5: %s (%s)\n", base.c_str(),v.c_str());
    if( base=="" ){
        sprintf( cres, "()");
    }
    else{ // anatomize cres: concat the velocity composition and substitute 0's with O's
        cres[iCres++]='(';
        for( int iBase=0; iBase<base.length(); iBase++){
            if( base[iBase]=='0' ){ // workaround as long as +0/00/-0 are not properly read
                cres[iCres++]='O';
                //printf( "SUBSTITUTE: %i |%s|\n", iBase, cres);
            } 
            else if( base[iBase]==' ' ){
                //std::cout << "Velocity = " << v << endl;
                if( v=="-" || v=="0" || v=="+" ){
                    if( v=="-" ) cres[iCres++]='-'; 
                    if( v=="0" ) cres[iCres++]='O'; // workaround as long as +0/00/-0 are not properly read
                    if( v=="+" ) cres[iCres++]='+';
                    cres[iCres++]=' ';
                }
                else{ // v={-,0,+}
                    cres[iCres++]='-';
                    cres[iCres++]=' ';
                    cres[iCres++]=cres[iCres-6];
                    cres[iCres++]=cres[iCres-6];
                    cres[iCres++]=cres[iCres-6];
                    cres[iCres++]=cres[iCres-6];
                    cres[iCres++]='O';
                    cres[iCres++]=cres[iCres-6];
                    cres[iCres++]=cres[iCres-6];
                    cres[iCres++]=cres[iCres-6];
                    cres[iCres++]=cres[iCres-6];
                    cres[iCres++]=cres[iCres-6];
                    cres[iCres++]='+';
                    cres[iCres++]=' ';
                }
            }
            else{
                cres[iCres++]=base[iBase];
            }
            if( iCres >= MAX_STRBUF){ 
                printf( "ATTENTION: BUFFER TO SMALL\n" );
                iBase=base.length(); // if so, end loop here
            }
        }
        cres[iCres++]=')';
        cres[iCres]=0;
    }
    // printf("Clean result by means of dellist ... TODO\n");
    cleanCres_qtc_c22();
    // printf( ">>>>>>>>>>>> %s <<<<<<<<<\n", cres);
    return cres;
}

/******************************************************************************
 ***  CONVERSE 
 ******************************************************************************/
const char* qtc_c22_converse(const char* foo, const char* rel_name)
{
    //printf( "INPUT CONVERSE C22: %s\n", rel_name);
    string relation( rel_name );
    string result( rel_name );
    
    /* Compute converse */
    // exchange values at position 0/1 and 2/3
    result.at(0)=relation.at(1);
    result.at(1)=relation.at(0);
    result.at(2)=relation.at(3);
    result.at(3)=relation.at(2);
    
    //inverse at position 4 
    result.at(4) = qtc_general_velocity_converse( result.at(4));
    
    if( result.at(0)=='0' ) result.at(0)='O';
    if( result.at(1)=='0' ) result.at(1)='O';
    if( result.at(2)=='0' ) result.at(2)='O';
    if( result.at(3)=='0' ) result.at(3)='O';
    if( result.at(4)=='0' ) result.at(4)='O';
    
    //printf( "RESULT CONVERSE C22: %s\n", result.c_str() );
    sprintf( cres, "%s", result.c_str() );
    
    return cres;
}


/******************************************************************************
 ******************************************************************************
 *** QTC-C21
 ******************************************************************************
 *****************************************************************************/
/******************************************************************************
 *** COMPOSITION
 *****************************************************************************/
const char* qtc_c21_composition(const char* foo, const char* rel_ab_name , const char* rel_bc_name){
    string sRel_AB( rel_ab_name );
    string sRel_BC( rel_bc_name );
    //printf( "INPUT C21_COMP: |%s| AND |%s|\n", rel_ab_name, rel_bc_name);
    
    // sprintf( sAB, "%s", rel_ab_name);
    // sprintf( sBC, "%s", rel_bc_name);
    // int length = min( sRel_AB.length(), sRel_BC.length());
    for(int i=0; i<4; i++){
        if( sRel_AB.at(i)=='O' || sRel_AB.at(i)=='o'){ sRel_AB[i]='0';}    
        if( sRel_BC.at(i)=='O' || sRel_BC.at(i)=='o'){ sRel_BC.at(i)='0';}
    }
    //printf( "INPUT MODIFIED: |%s| AND |%s|\n", sRel_AB.c_str(), sRel_BC.c_str());
    
    string v1 = sRel_AB.substr(4,1);
    string v2 = sRel_BC.substr(4,1);
    string rel_ab( sRel_AB );
    string rel_bc( sRel_BC );
    string base = getQTCcomposition( sRel_AB, sRel_BC );
    int iCres=0;
    
    //printf( "********************************************** 5: %s \n", base.c_str());
    if( base=="" ){
        sprintf( cres, "()");
    }
    else{ // anatomize cres: concat the velocity composition and substitute 0's with O's
        cres[iCres++]='(';
        for( int iBase=0; iBase<base.length(); iBase++){
            if( base[iBase]=='0' ){ // workaround as long as +0/00/-0 are not properly read
                cres[iCres]='O';
                //printf( "SUBSTITUTE: %i |%s|\n", iBase, cres);
            } 
            else{
                cres[iCres]=base[iBase];
            }
            if( iCres >= MAX_STRBUF){ 
                printf( "ATTENTION: BUFFER TO SMALL\n" );
                iBase=base.length(); // if so, end loop here
            }
            iCres++;
        }
        cres[iCres-1]=')';
        cres[iCres++]=0;
    }
    
    //printf( ">>>>>>>>>>>> %s <<<<<<<<<\n", cres);
    return cres;
}

/******************************************************************************
 ***  CONVERSE 
 ******************************************************************************/
const char* qtc_c21_converse(const char* foo, const char* rel_name)
{
    //printf( "INPUT C21 CONVERSE: %s\n", rel_name);
    sprintf( cres, "%c%c%c%c\0", rel_name[1], rel_name[0], rel_name[3], rel_name[2]);
    return cres;
}

/******************************************************************************
 ******************************************************************************
 *** QTC-B21
 ******************************************************************************
 *****************************************************************************/
/******************************************************************************
 *** COMPOSITION
 *****************************************************************************/
const char* qtc_b21_composition(const char* foo, const char* rel_ab_name , const char* rel_bc_name){
	// bool expr = true if b21 Relation is contained in result (cmp. aB21Relations[])
	bool abRelations[9] = { 0,0,0,0,0,0,0,0,0 }; 
    
	char rel_ab[10]= "\0";
	char rel_bc[10]= "\0";
	// printf( "INPUT B21_COMP: |%s| AND |%s|\n", rel_ab_name, rel_bc_name);
	for( int c1=0; c1<3; c1++){
		for( int d1=0; d1<3; d1++){
			for( int c2=0; c2<3; c2++){
				for( int d2=0; d2<3; d2++){
					sprintf( rel_ab, "%c%c%c%c", rel_ab_name[0],rel_ab_name[1], symbol[c1],symbol[d1]);
					sprintf( rel_bc, "%c%c%c%c", rel_bc_name[0],rel_bc_name[1], symbol[c2],symbol[d2]);
					// printf( "INPUT MODIFIED B21_COMP: |%s| AND |%s|\n", rel_ab, rel_bc);
					string sResult( qtc_c21_composition( foo, rel_ab , rel_bc) );
					// scan thru result of C21 tokens (4*char) for B21 tokens (1st two chars of C21 tokens)
					// printf( "sResult: %s\n", sResult.c_str() );
					for( int i=0; i<sResult.length(); i++){
						// printf("i=%i: %s\n", i, sResult.substr(i,1).c_str());
						if( sResult.substr(i,1)=="+" || sResult.substr(i,1)=="0" || sResult.substr(i,1)=="-" ||
                           sResult.substr(i,1)=="O" || sResult.substr(i,1)=="o" ){ // now at 1st position of C21 token
							string sTmp = sResult.substr(i,2);
							for( int j=0; j<9; j++ ){
								if( sTmp==aB21relations0[j] || sTmp==aB21relationsO[j]){ 
									abRelations[j]=true;
									// printf( "%s => %s = %i (index: %i)\n", sTmp.c_str(), aB21relationsO[j].c_str(), abRelations[j],j);
									j=9;
								}
							}
							i+=3;
							//printf( "Now i=%i\n", i);
						}
						//else just go on
					}
				}
			}
		}
	}
	string sResult="";
	for( int i=0; i<9; i++){
		if( abRelations[i]==true ){
			sResult = sResult + aB21relationsO[i] + " ";
		}
	}
	sprintf( cres, "(%s)", sResult.c_str());
	//printf( "cres: %s\n", cres);
	return cres;
}
/******************************************************************************
 ***  CONVERSE 
 ******************************************************************************/
const char* qtc_b21_converse(const char* foo, const char* rel_name){
    //printf( "INPUT B21 CONVERSE: %s\n", rel_name);
    sprintf( cres, "%c%c\0", rel_name[1], rel_name[0]);
    //printf( "RESULT CONVERSE: %s", result.c_str() );
    
    return cres;
}


/******************************************************************************
 ******************************************************************************
 *** QTC-B22
 ******************************************************************************
 *****************************************************************************/
/******************************************************************************
 *** COMPOSITION
 *****************************************************************************/
const char* qtc_b22_composition(const char* foo, const char* rel_ab_name , const char* rel_bc_name){
	string sRel_AB( rel_ab_name );
	string sRel_BC( rel_bc_name );
	string v1 = sRel_AB.substr(2,1);
	string v2 = sRel_BC.substr(2,1);
	sprintf(cres, "");
	//printf( "INPUT B22 COMP: |%s| AND |%s|\n", rel_ab_name, rel_bc_name);
	string v = qtc_general_velocity_composition( v1, v2 );  
    if( !(v=="-" || v=="+" || v!="0" || v!="O" || v!="o" || v!="A")) {
        printf( "ERROR in velocity part of composition (%s)... EXIT!!!!\n", v.c_str() );
        exit(EXIT_FAILURE);
    }
	string base = qtc_b21_composition(foo, rel_ab_name , rel_bc_name);
	int iCres=0;  
	//printf( "beween: |%s| AND |%s|\n", base.c_str(), v.c_str());
	if( base=="" ){
		sprintf( cres, "()");
	}
	else { // anatomize cres: concat the velocity composition and substitute 0's with O's
        //cres[iCres++]='(';
		for( int iBase=0; iBase<base.length(); iBase++){
			//printf( "%i:cres: %s\n", iBase, cres);
			if( base[iBase]=='0' ){ // workaround as long as +0/00/-0 are not properly read
				cres[iCres++]='O';
				// printf( "SUBSTITUTE: %i |%s|\n", iBase, cres);
			} 
			else if( base[iBase]==' ' ){
				if( v=="-" || v=="0" || v=="+" ){
					if( v=="-" ) cres[iCres++]='-'; 
					if( v=="0" ) cres[iCres++]='O'; // workaround as long as +0/00/-0 are not properly read
					if( v=="+" ) cres[iCres++]='+';
					cres[iCres++]=' ';
				}
				else{ // v={-,0,+}
					cres[iCres++]='-';
					cres[iCres++]=' ';
					cres[iCres++]=cres[iCres-4];
					cres[iCres++]=cres[iCres-4];
					cres[iCres++]='O';
					cres[iCres++]=cres[iCres-4];
					cres[iCres++]=cres[iCres-4];
					cres[iCres++]=cres[iCres-4];
					cres[iCres++]='+';
					cres[iCres++]=' ';
				}
			}
			else {
				cres[iCres++]=base[iBase];
			}
			if( iCres >= MAX_STRBUF){ 
				printf( "ATTENTION: BUFFER TO SMALL\n" );
				iBase=base.length(); // if so, end loop here
			}
		}
		//cres[iCres++]=')';
		cres[iCres]=0; 
	}
    return cres;
}
/******************************************************************************
 ***  CONVERSE 
 ******************************************************************************/
const char* qtc_b22_converse(const char* foo, const char* rel_name){
    //printf( "INPUT B22 CONVERSE: %s\n", rel_name);
    char v =   qtc_general_velocity_converse( rel_name[2]);
    if( v=='0' ){ v='O'; } // Workaround for 0/O problem
    sprintf( cres, "%c%c%c\0", rel_name[1], rel_name[0],v);
    //printf( "CRES CONVERSE: %s (%c)\n", cres, v);
    return cres;
}



/******************************************************************************
 ******************************************************************************
 *** MAIN
 ******************************************************************************
 *****************************************************************************/
int main( int argc, char ** argv ) {
    
    string r1, r2;
    char * tmp;
    
    bool bQTC_C22 = false;
    bool bQTC_C21 = false;
    bool bQTC_B22 = true;
    bool bQTC_B21 = true;
    
    if (argc==3) {
        r1 = argv[1];
        r2 = argv[2];
        if( bQTC_C22 ){
            cout << endl << "*** QTC_C22 ***" << endl;
            //cout << printComp(lookUpTable(argv[1], argv[2])) << endl;
            cout << "Composition_C22: " << qtc_c22_composition( tmp, argv[1], argv[2]) << endl;
            cout << "Converse_C22 (" << argv[1] << "): " << qtc_c22_converse( tmp, argv[1]) << endl;
            cout << "Converse_C22 (" << argv[2] << "): " << qtc_c22_converse( tmp, argv[2]) << endl;
        }
        if( bQTC_C21 ){
            cout << endl << "*** QTC_C21 ***" << endl;
            cout << "Composition_C21: " << qtc_c21_composition( tmp, argv[1], argv[2]) << endl;
            cout << "Converse_C21 (" << argv[1] << "): " << qtc_c21_converse( tmp, argv[1]) << endl;
            cout << "Converse_C21 (" << argv[2] << "): " << qtc_c21_converse( tmp, argv[2]) << endl;
        }
        if( bQTC_B22 ){
            cout << endl << "*** QTC_B22 ***" << endl;
            cout << "Composition_B22: " << qtc_b22_composition( tmp, argv[1], argv[2]) << endl;
            cout << "Converse_B22 (" << argv[1] << "): " << qtc_b22_converse( tmp, argv[1]) << endl;
            cout << "Converse_B22 (" << argv[2] << "): " << qtc_b22_converse( tmp, argv[2]) << endl;
        }
        if( bQTC_B21 ){
            cout << endl << "*** QTC_B21 ***" << endl;
            cout << "Composition_B21    : " << qtc_b21_composition( tmp, argv[1], argv[2]) << endl;
            cout << "Converse_B21 (" << argv[1] << "): " << qtc_b21_converse( tmp, argv[1]) << endl;
            cout << "Converse_B21 (" << argv[2] << "): " << qtc_b21_converse( tmp, argv[2]) << endl;
        }
    }
    return 0;
}

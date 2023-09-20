#include<iostream>
#include<fstream>
#include<vector>
#include<algorithm>
#include<valarray>
#include<sstream>
#include<unistd.h>
#include<stdlib.h>
using namespace std;

string formatnum(int len, int value){
	stringstream str;
    str << hex; str.fill('0');
	if(value >= 0x100) cout << "error" << endl;
	else if(value >= 0x10) str << "" << value;
	else str << "0" << value;
	return str.str();
}
string getcolor(int c1, int c2, int c3){
    stringstream s;
    s << "#" << formatnum(2, c1) << formatnum(2, c2) << formatnum(2, c3);
    return s.str();
}
enum class Shapes{
    set_size,
    line,
    circle,
    rect,
    tri,
};
string getsvg(vector<int> inputs){
    int width = 100;
    int height = 100;
    stringstream svg;
    int i = 0;
    while(i < inputs.size()){
        switch(inputs[i]){
            case (int)Shapes::set_size:
                width = inputs[i + 1];
                height = inputs[i + 2];
                i += 3;
                break;
            case (int)Shapes::line:
                svg << "<line x1=\"" << inputs[i + 1] << "\" y1=\"" << inputs[i + 2] << "\" x2=\"" << inputs[i + 3] << "\" y2=\"" << inputs[i + 4]
                    << "\" stroke=\"" << getcolor(inputs[i + 5] , inputs[i + 6], inputs[i + 7]) << "\" />" << endl;
                i += 8;
                break;
            case (int)Shapes::circle:
                svg << "<circle cx=\"" << inputs[i + 1] << "\" cy=\"" << inputs[i + 2] << "\" r=\"" << inputs[i + 3]
                    << "\" fill=\"" << getcolor(inputs[i + 4] , inputs[i + 5], inputs[i + 6]) << "\" />" << endl;
                i += 7;
                break;
            case (int)Shapes::rect:
                svg << "<rect x=\"" << inputs[i + 1] << "\" y=\"" << inputs[i + 2] << "\" width=\"" << inputs[i + 3] << "\" height=\"" << inputs[i + 4]
                    << "\" fill=\"" << getcolor(inputs[i + 5] , inputs[i + 6], inputs[i + 7]) << "\" />" << endl;
                i += 8;
                break;
            case (int)Shapes::tri:
                svg << "<path d=\"M " << inputs[i + 1] << " " << inputs[i + 2] << " L " << inputs[i + 3] << " " << inputs[i + 4] << " L " << inputs[i + 5] << " " << inputs[i + 6] << " z\""
                    << " fill=\"" << getcolor(inputs[i + 7] , inputs[i + 8], inputs[i + 9]) << "\" />" << endl;
                i += 10;
                break;
            default:
                cout << "error" << endl;
        }
    }
    stringstream ret;
    ret << "<svg width=\"" << width << "\" height=\"" << height << "\" viewBox=\"0, 0, " << width << ", " << height << "\" xmlns=\"http://www.w3.org/2000/svg\">" << svg.str() << "</svg>";
    return ret.str();
}
int main(int argc, char* argv[]){
    string out_file_name = argv[1];
    bool is_print = atoi(argv[2]);

    ofstream output(out_file_name);
    if(output.fail()){
        cout << "ファイルが開けません" << endl;
        return 1;
    }

    vector<int> inputs;
    for(int i = 4; i < argc; i+=2){
        int input = atoi(argv[i]);
        inputs.push_back(input);
    }

    string svg = getsvg(inputs);
    if(is_print)
        cout << svg << endl;
    output << svg << endl;
    
    output.close();
    return 0;
}
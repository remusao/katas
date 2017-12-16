#include <iostream>
#include <string>


int main() {
    std::string time;
    std::cin >> time;
    bool pm = time[time.size() - 2] == 'P';
    bool am = !pm;

    if (time[0] == '1' && time[1] == '2') {
        if (am) {
            time[0] = '0';
            time[1] = '0';
        }
    }
    else if (pm) {
        time[0] += 1;
        time[1] += 2;
    }

    time.resize(time.size() - 2);
    std::cout << time << std::endl;

    return 0;
}

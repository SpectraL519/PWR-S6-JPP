#include "dining_philosophers_list.hpp"

#include "philosopher.hpp"

#include <algorithm>
#include <iostream>

namespace jpp {

void dining_philosophers_list::add_philosopher(philosopher& p) {
    std::lock_guard<std::mutex> lock_guard{this->_mutex};
    this->_philosophers.push_back(std::ref(p));
    this->_print();
}

void dining_philosophers_list::remove_philosopher(const uint16_t p_id) {
    std::lock_guard<std::mutex> lock_guard{this->_mutex};
    std::erase_if(this->_philosophers, [p_id](philosopher& p){ return p.id() == p_id; });
    this->_print();
}

void dining_philosophers_list::_print() const {
    std::cout << "> ";
    std::for_each(
        this->_philosophers.cbegin(),
        this->_philosophers.cend(),
        [](const auto& pw) { std::cout << pw.get().to_string() << " "; }
    );
    std::cout << "\n";
}

} // namespace jpp

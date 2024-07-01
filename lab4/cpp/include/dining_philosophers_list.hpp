#pragma once

#include <cstdint>
#include <functional>
#include <mutex>
#include <vector>

namespace jpp {

class philosopher;

class dining_philosophers_list {
public:
    dining_philosophers_list() = default;
    ~dining_philosophers_list() = default;

    void add_philosopher(philosopher& p);
    void remove_philosopher(const uint16_t p_id);

private:
    void _print() const;

    std::mutex _mutex;
    std::vector<std::reference_wrapper<philosopher>> _philosophers;
};

} // namespace jpp

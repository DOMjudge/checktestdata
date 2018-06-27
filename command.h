#pragma once

class Command {
 public:
  virtual ~Command() {}
  virtual void run(std::string_view& in) { next_->run(in); };

  Command* next_ = nullptr;
};

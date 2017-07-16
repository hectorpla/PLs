# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  Extra_Pieces = [rotations([[0, 0], [-1, 0], [1, 0], [0, 1], [-1, 1]]),
                  [[[0, 0], [-1, 0], [1, 0], [-2, 0], [2, 0]],
                   [[0, 0], [0, 1], [0, -1], [0, 2], [0, -2]]],
                  rotations([[0,0], [1, 0], [0, 1]])]
  All_My_Pieces = All_Pieces + Extra_Pieces
  # your enhancements here
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheat_piece (board)
    MyPiece.new([[[0, 0]]], board)
  end
end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    super
    @current_block = MyPiece.next_piece(self) # next called twice
    @cheat = false
  end

  def do_cheat
    if !@cheat and score >= 100
      @score -= 100
      @cheat = true
    end
    @game.update_score
  end
  
  def rotate_upside_down
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def next_piece
    if @cheat
      @current_block = MyPiece.cheat_piece(self)
      @cheat = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end
  
  def store_current
    # copy paste
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..locations.length-1).each{|index| # modified
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

class MyTetris < Tetris
  # your enhancements here
  def initialize
    # copy paste, bad?
    @cheat = false # new added
    @root = TetrisRoot.new
    @timer = TetrisTimer.new
    set_board
    @running = true
    key_bindings
    extra_key_bindings # new added
    buttons
    run_game
  end

  def set_board
    # copy paste
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def extra_key_bindings
    @root.bind('u', proc {@board.rotate_upside_down})
    @root.bind('c', proc {@board.do_cheat})
  end
end


package beetle.brindi.sudoku.controller;

import beetle.brindi.sudoku.domain.Sudoku;
import beetle.brindi.sudoku.service.SudokuService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/v1/sudoku")
@RequiredArgsConstructor
public class SudokuController {

    private final SudokuService sudokuService;

    @GetMapping("/random")
    public Sudoku getRandom() {
        return sudokuService.getRandomSudoku();
    }
}

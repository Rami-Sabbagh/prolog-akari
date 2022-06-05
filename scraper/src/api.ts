import client from './client';

export class ParseError extends Error {
    constructor() {
        super('Parse error');
    }
}

export class PuzzleNotFoundError extends Error {
    constructor(message = 'Puzzle not found') {
        super(message);
    }
}

export interface PuzzleData {
    /**
     * The title of the pizzle.
     * 
     * e.x: `'KD_Akari_8x8_V1-B1-P1'`
     */
    ptitle: string,
    /**
     * e.x: `.1#..12..........##..1#..................#1..#2..........##..##.`
     */
    puzz: string,
    /**
     * e.x: `1000100100000010000000000000010000000010000100000000001000000000`
     */
    solved: string,
    /**
     * e.x: 5
     */
    work: number,
    /**
     * e.x: 8
     */
    width: number,
    /**
     * e.x: 8
     */
    height: number,
}

interface PuzzleResponse {
    puzzle_data: PuzzleData,
    puzzle_id: string,
    success: boolean,
    error?: boolean,
    message?: string,
}

export async function fetchLevel(dimension = 8, puzzle = 1, book = 1, volume = 1): Promise<PuzzleData> {
    const response = await client.get<string>('', {
        params: {
            kind: `${dimension}x${dimension}`,
            volumeNumber: volume,
            bookNumber: book,
            puzzleNumber: puzzle,
        },
    });
    const rawResult = response.data.match(/var pRec = (?<json>[^;]*);/)?.groups?.json;
    if (!rawResult) throw new ParseError();

    const result: PuzzleResponse = JSON.parse(rawResult);

    if (result.success) return result.puzzle_data;
    throw new PuzzleNotFoundError(result.message);
}
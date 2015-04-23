#include <iostream>
#include <string>
#include <bitset>
#include <vector>
#include <random>
#include <utility>
#include <list>
#include <algorithm>
#include <stdexcept>
#include <boost/optional.hpp>
namespace prisoner_dilemma
{
    constexpr size_t cooperate_gain( 3 );
    constexpr size_t defect_gain( 5 );
    constexpr size_t defected_gain( 0 );
    constexpr size_t dis_cooperate_gain( 1 );
    constexpr double cross_over_rate( 0 );
    constexpr double mutate_rate( 0 );
    constexpr size_t power( size_t x, size_t y )
    { return y == 0 ? x == 0 ? 0 : 1 :x * power( x, y - 1 ); }
    template< size_t memory >
    struct gene
    {
        std::bitset< power( 4, memory ) + ( 2 * memory ) > bsv;
        gene( ) { for ( size_t i = 0; i < bsv.size( ); ++i ) { bsv[ i ] = ( rand( ) % 2 == 0 ); } }
        bool will_cooperate( std::list< std::pair< bool, bool > > & history ) const
        {
            if ( history.empty( ) )
            {
                size_t i = power( 4, memory );
                while ( i < bsv.size( ) )
                {
                    history.push_back( std::make_pair( bsv[ i ], bsv[ i + 1 ] ) );
                    i+=2;
                }
            }
            size_t position( 0 );
            for ( auto i = history.begin( ); i != history.end( ); ++i )
            {
                position *= 4;
                position += i->first * 2 + i->second;
            }
            return bsv[ position ];
        }

        void cross_over( gene & rhs )
        {
            if ( bsv.size( ) <= 1 ) return;
            size_t cross_point = std::random_device( )( ) % ( bsv.size( ) - 1 ) + 1;
            while ( cross_point < bsv.size( ) )
            {
                bool tem( bsv[ cross_point ] );
                bsv[ cross_point ] = rhs.bsv[ cross_point ];
                rhs.bsv[ cross_point ] = tem;
                ++cross_point;
            }
        }

        void mutate( double mutate_rate )
        {
            std::random_device rd;
            for ( size_t i = 0; i < bsv.size( ); ++i )
            {
                auto res( rd( ) );
                if ( static_cast< double >( res ) / ( rd.max( ) - rd.min( ) ) < mutate_rate )
                { bsv[ i ] = ! bsv[ i ]; }
            }
        }

        static gene< memory > make_always_cooperate( )
        {
            gene< memory > ret;
            ret.bsv.set( );
            return ret;
        }

        static gene< memory > make_always_defect( )
        {
            gene< memory > ret;
            ret.bsv.reset( );
            return ret;
        }

        static gene< 1 > make_tit_for_tat( )
        {
            gene< memory > ret;
            ret.bsv[0] = false;
            ret.bsv[1] = true;
            ret.bsv[2] = false;
            ret.bsv[3] = true;
            ret.bsv[4] = true;
            ret.bsv[5] = true;
            return ret;
        }

        static std::pair< size_t, size_t > game( const size_t match_turn, const gene & lhs, const gene & rhs )
        {
            size_t lhs_res( 0 ), rhs_res( 0 );
            std::list< std::pair< bool, bool > > lhs_his, rhs_his;
            for ( size_t i = 0; i < match_turn; ++i )
            {
                bool lhs_move( lhs.will_cooperate( lhs_his ) ), rhs_move( rhs.will_cooperate( rhs_his ) );
                if ( lhs_move && rhs_move )
                {
                    lhs_res += cooperate_gain;
                    rhs_res += cooperate_gain;
                    lhs_his.push_back( std::make_pair( true, true ) );
                    rhs_his.push_back( std::make_pair( true, true ) );
                    lhs_his.pop_front( );
                    rhs_his.pop_front( );
                }
                else if ( ( ! lhs_move ) && ( ! rhs_move ) )
                {
                    lhs_res += dis_cooperate_gain;
                    rhs_res += dis_cooperate_gain;
                }
                else
                {
                    lhs_res += lhs_move ? defected_gain : defect_gain;
                    rhs_res += rhs_move ? defected_gain : defect_gain;
                }
                lhs_his.push_back( std::make_pair( lhs_move, rhs_move ) );
                rhs_his.push_back( std::make_pair( rhs_move, lhs_move ) );
                lhs_his.pop_front( );
                rhs_his.pop_front( );
            }
            return std::make_pair( lhs_res, rhs_res );
        }
        bool operator == ( const gene & com ) const { return bsv == com.bsv; }
    };

    template< size_t memory >
    struct genetic_pool
    {
        std::vector< gene< memory > > vg;
        boost::optional< gene< memory > > best;
        std::pair< double, size_t > to_next_generation( )
        {
            std::vector< size_t > result;
            result.reserve( vg.size( ) );
            for ( auto beg = vg.begin( ); beg != vg.end( ); beg+=2 )
            {
                auto res = gene< memory >::game( 10, * beg, * ( beg + 1 ) );
                result.push_back( res.first );
                result.push_back( res.second );
            }
            size_t total_result = accumulate( result.begin( ), result.end( ), 0 );
            if ( total_result == 0 ) { throw std::runtime_error( "total fitness is 0, hence unable to do roulette selection" ); }
            std::vector< gene< memory > > next_generation;
            next_generation.reserve( vg.size( ) );
            std::random_device rd;
            best = * ( vg.begin( ) + std::distance( result.begin( ), std::max_element( result.begin( ), result.end( ) ) ) );

            for( size_t i = 0; i < vg.size( ); ++i )
            {
                size_t pointer_point_to = rd( ) % total_result;
                for ( size_t ii = 0; ; ++ii )
                {
                    if ( pointer_point_to >= result[ ii ] ) { pointer_point_to -= result[ ii ]; }
                    else { next_generation.push_back( vg[ ii ] ); break; }
                }
            }

            next_generation.swap( vg );

            for ( size_t i = 0; i < vg.size( ); i += 2 )
            {
                if( static_cast< double >( rd( ) ) / ( rd.max( ) - rd.min( ) ) < cross_over_rate )
                { vg[ i ].cross_over( vg[ i + 1 ] ); }
            }

            for ( size_t i = 0; i < vg.size( ); ++i ) { vg[ i ].mutate( mutate_rate ); }

            return std::make_pair( static_cast< double >( total_result ) / vg.size( ), * max_element( result.begin( ), result.end( ) ) );
        }
        genetic_pool( size_t size ) : vg( size ) { }
    };

    template< size_t memory >
    void report_match( const gene< memory > & l, const gene< memory > & r, const std::string & l_name, const std::string & r_name )
    {
        auto res = gene< memory  >::game( 100, l, r );
        std::cout << "a game between " << l_name << " and " << r_name << ":" << std::endl
                            << "score of" << l_name << ":"
                            << res.first << std::endl
                            << "score of" << r_name << ":"
                            << res.second << std::endl;
    }

    void example( )
    {
        constexpr size_t size = 200;
        auto test =
            [](size_t num_tit_for_tat)
            {
                genetic_pool< 1 > gp( size );
                size_t i = 0;
                for ( ; i < num_tit_for_tat; ++i ) { gp.vg[i] = gene< 1 >::make_tit_for_tat( ); }
                for ( ; i < size; ++i ) { gp.vg[i] = gene< 1 >::make_always_defect( ); }
                std::random_shuffle( gp.vg.begin( ), gp.vg.end( ) );
                gp.to_next_generation( );
                return std::count( gp.vg.cbegin( ), gp.vg.cend( ), gene< 1 >::make_tit_for_tat( ) );
            };
        size_t num_test = 200;
        for ( size_t i = 0; i < size; ++i )
        {
            size_t acc = 0;
            for ( size_t ii = 0; ii < num_test; ++ii ) { acc += test( i ); }
            std::cout
                    << "inital num:" << i
                    << "final num:"
                    << static_cast< double >( acc )/num_test
                    << "growth rate:" << (static_cast< double >( acc )/num_test)/i
                    << std::endl;
        }
    }
}

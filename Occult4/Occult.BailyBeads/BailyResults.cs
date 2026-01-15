using System;
using System.Text;

namespace Occult.BailyBeads
{
	internal class BailyResults : IComparable
	{
		private bool check;

		private int hr;

		private int min;

		private double sec;

		private double aa;

		private double height;

		internal static int SortField;

		public int Hr
		{
			get
			{
				return hr;
			}
			set
			{
				hr = value;
			}
		}

		public int Min
		{
			get
			{
				return min;
			}
			set
			{
				min = value;
			}
		}

		public double Sec
		{
			get
			{
				return sec;
			}
			set
			{
				sec = value;
			}
		}

		public double AA
		{
			get
			{
				return aa;
			}
			set
			{
				aa = value;
			}
		}

		public double Height
		{
			get
			{
				return height;
			}
			set
			{
				height = value;
			}
		}

		public bool Check
		{
			get
			{
				return check;
			}
			set
			{
				check = value;
			}
		}

		public double EventTimeHrs => (double)hr + (double)min / 60.0 + sec / 3600.0;

		public string TextLine => ShortLine();

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendFormat("{0,2:F0}", hr);
			stringBuilder.AppendFormat("{0,3:F0}", min);
			stringBuilder.AppendFormat("{0,6:F2}", sec);
			stringBuilder.AppendFormat("{0,8:F2}", aa);
			stringBuilder.AppendFormat("{0,7:F2}", height);
			if (check)
			{
				stringBuilder.Append(" 1");
			}
			else
			{
				stringBuilder.Append(" 0");
			}
			return stringBuilder.ToString();
		}

		private string ShortLine()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendFormat("{0,2:F0}", hr);
			stringBuilder.AppendFormat("{0,3:F0}", min);
			stringBuilder.AppendFormat("{0,6:F2}", sec);
			stringBuilder.AppendFormat("{0,8:F2}", aa);
			stringBuilder.AppendFormat("{0,7:F2}", height);
			return stringBuilder.ToString();
		}

		public int CompareTo(object other)
		{
			return SortField switch
			{
				0 => EventTimeHrs.CompareTo(((BailyResults)other).EventTimeHrs), 
				1 => AA.CompareTo(((BailyResults)other).AA), 
				_ => 0, 
			};
		}

		internal void ReadLine(string InLine)
		{
			if (!int.TryParse(InLine.Substring(0, 2), out hr))
			{
				hr = 0;
			}
			if (!int.TryParse(InLine.Substring(3, 2), out min))
			{
				min = 0;
			}
			if (!double.TryParse(InLine.Substring(6, 5), out sec))
			{
				sec = 0.0;
			}
			if (!double.TryParse(InLine.Substring(12, 7), out aa))
			{
				aa = 0.0;
			}
			if (!double.TryParse(InLine.Substring(19, 7), out height))
			{
				height = 0.0;
			}
			check = InLine.Substring(27, 1) == "1";
		}
	}
}

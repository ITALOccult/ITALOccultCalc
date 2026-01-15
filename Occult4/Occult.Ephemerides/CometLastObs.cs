using System;

namespace Occult.Ephemerides
{
	internal class CometLastObs : IComparable
	{
		private string id = "";

		private string name = "";

		private bool isPeriodic;

		private bool isNumbered;

		private DateTime lastObsDate;

		private float lastMag = 22f;

		internal string ID
		{
			get
			{
				return id;
			}
			set
			{
				id = value;
			}
		}

		internal string Name
		{
			get
			{
				return name;
			}
			set
			{
				name = value;
			}
		}

		internal bool IsPeriodic
		{
			get
			{
				return isPeriodic;
			}
			set
			{
				isPeriodic = value;
			}
		}

		internal bool IsNumbered
		{
			get
			{
				return isNumbered;
			}
			set
			{
				isNumbered = value;
			}
		}

		internal DateTime LastObsDate
		{
			get
			{
				return lastObsDate;
			}
			set
			{
				lastObsDate = value;
			}
		}

		internal float LastMag
		{
			get
			{
				return lastMag;
			}
			set
			{
				lastMag = value;
			}
		}

		internal void DecodeCometLastObs(string InLine)
		{
			int num = InLine.IndexOf("P-");
			if (num > 0)
			{
				num += 2;
			}
			if (num < 0)
			{
				num = InLine.IndexOf("P/");
			}
			int num2 = 0;
			int num3 = 0;
			int result = 0;
			int result2 = 0;
			int result3 = 0;
			float result4 = 0f;
			if (num > 0 && num < 7)
			{
				isPeriodic = (isNumbered = true);
				id = InLine.Substring(0, num + 1).PadLeft(4);
				num2 = InLine.IndexOf("/");
				name = InLine.Substring(num2 + 1, 32 - num).Trim();
			}
			else
			{
				isNumbered = false;
				isPeriodic = InLine.PadRight(1).Substring(0, 1) == "P";
				num2 = InLine.IndexOf("(");
				if (num2 > 0)
				{
					num3 = InLine.IndexOf(")");
					if (num3 < 0)
					{
						num3 = 36;
					}
					id = InLine.Substring(2, num2 - 2).Trim();
					name = InLine.Substring(num2 + 1, num3 - num2 - 1);
				}
				else
				{
					id = InLine.Substring(2, 33).Trim();
					name = "";
				}
			}
			if (!int.TryParse(InLine.Substring(36, 4), out result))
			{
				lastObsDate = new DateTime(1700, 1, 1);
				lastMag = 25f;
				return;
			}
			if (!int.TryParse(InLine.Substring(41, 2), out result2))
			{
				result2 = 1;
			}
			if (!int.TryParse(InLine.Substring(44, 2), out result3))
			{
				result3 = 1;
			}
			lastObsDate = new DateTime(result, result2, result3);
			if (!float.TryParse(InLine.Substring(51, 4), out result4))
			{
				result4 = 25f;
			}
			LastMag = result4;
		}

		public int CompareTo(object other)
		{
			return ID.CompareTo(((CometLastObs)other).ID);
		}

		public override string ToString()
		{
			if (isNumbered)
			{
				return ID + "P/" + Name;
			}
			if (IsPeriodic)
			{
				return "P/" + ID + " (" + Name + ")";
			}
			return "C/" + ID + " (" + Name + ")";
		}
	}
}
